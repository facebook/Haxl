-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Implementation of data-fetching operations.  Most users should
-- import "Haxl.Core" instead.
--
module Haxl.Core.Fetch
  ( dataFetch
  , dataFetchWithShow
  , uncachedRequest
  , cacheResult
  , dupableCacheRequest
  , cacheResultWithShow
  , cacheRequest
  , performFetches
  , performRequestStore
  , ShowReq
  ) where

import Control.Concurrent.STM
import Control.Exception as Exception
import Control.Monad
import Data.Either
import Data.Hashable
import Data.IORef
import Data.Int
import Data.List
#if __GLASGOW_HASKELL__ < 804
import Data.Monoid
#endif
import Data.Proxy
import Data.Typeable
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Printf
#ifdef PROFILING
import GHC.Stack
#endif

import Haxl.Core.DataSource
import Haxl.Core.DataCache as DataCache
import Haxl.Core.Exception
import Haxl.Core.Flags
import Haxl.Core.Monad
import Haxl.Core.Profile
import Haxl.Core.RequestStore
import Haxl.Core.ShowP
import Haxl.Core.Stats
import Haxl.Core.StateStore
import Haxl.Core.Util

-- -----------------------------------------------------------------------------
-- Data fetching and caching

-- | Possible responses when checking the cache.
data CacheResult u w a
  -- | The request hadn't been seen until now.
  = Uncached
       (ResultVar a)
       {-# UNPACK #-} !(IVar u w a)

  -- | The request has been seen before, but its result has not yet been
  -- fetched.
  | CachedNotFetched
      {-# UNPACK #-} !(IVar u w a)

  -- | The request has been seen before, and its result has already been
  -- fetched.
  | Cached (ResultVal a w)


-- | Show functions for request and its result.
type ShowReq r a = (r a -> String, a -> String)

-- Note [showFn]
--
-- Occasionally, for tracing purposes or generating exceptions, we need to
-- call 'show' on the request in a place where we *cannot* have a Show
-- dictionary. (Because the function is a worker which is called by one of
-- the *WithShow variants that take explicit show functions via a ShowReq
-- argument.) None of the functions that does this is exported, so this is
-- hidden from the Haxl user.

cachedWithInsert
  :: forall r a u w.
     (DataSource u r, Typeable (r a))
  => (r a -> String)    -- See Note [showFn]
  -> (r a -> IVar u w a -> DataCache (IVar u w) -> IO ())
  -> Env u w -> r a -> IO (CacheResult u w a)
cachedWithInsert showFn insertFn Env{..} req = do
  let
    doFetch = do
      ivar <- newIVar
      let !rvar = stdResultVar ivar completions submittedReqsRef flags
            (Proxy :: Proxy r)
      insertFn req ivar dataCache
      return (Uncached rvar ivar)
  mbRes <- DataCache.lookup req dataCache
  case mbRes of
    Nothing -> doFetch
    Just (IVar cr) -> do
      e <- readIORef cr
      case e of
        IVarEmpty _ -> return (CachedNotFetched (IVar cr))
        IVarFull r -> do
          ifTrace flags 3 $ putStrLn $ case r of
            ThrowIO{} -> "Cached error: " ++ showFn req
            ThrowHaxl{} -> "Cached error: " ++ showFn req
            Ok{} -> "Cached request: " ++ showFn req
          return (Cached r)


-- | Make a ResultVar with the standard function for sending a CompletionReq
-- to the scheduler. This is the function will be executed when the fetch
-- completes.
stdResultVar
  :: forall r a u w. (DataSourceName r, Typeable r)
  => IVar u w a
  -> TVar [CompleteReq u w]
  -> IORef ReqCountMap
  -> Flags
  -> Proxy r
  -> ResultVar a
stdResultVar ivar completions ref flags p =
  mkResultVar $ \r isChildThread -> do
    allocs <- if isChildThread
      then
        -- In a child thread, return the current allocation counter too,
        -- for correct tracking of allocation.
        getAllocationCounter
      else
        return 0
    -- Decrement the counter as request has finished
    ifReport flags 1 $
      atomicModifyIORef' ref (\m -> (subFromCountMap p 1 m, ()))
    atomicallyOnBlocking
      (LogicBug (ReadingCompletionsFailedFetch (dataSourceName p))) $ do
      cs <- readTVar completions
      writeTVar completions (CompleteReq r ivar allocs : cs)
{-# INLINE stdResultVar #-}


-- | Record the call stack for a data fetch in the Stats.  Only useful
-- when profiling.
logFetch :: Env u w -> (r a -> String) -> r a -> IO ()
#ifdef PROFILING
logFetch env showFn req = do
  ifReport (flags env) 5 $ do
    stack <- currentCallStack
    modifyIORef' (statsRef env) $ \(Stats s) ->
      Stats (FetchCall (showFn req) stack : s)
#else
logFetch _ _ _ = return ()
#endif

-- | Performs actual fetching of data for a 'Request' from a 'DataSource'.
dataFetch :: (DataSource u r, Request r a) => r a -> GenHaxl u w a
dataFetch = dataFetchWithInsert show DataCache.insert

-- | Performs actual fetching of data for a 'Request' from a 'DataSource', using
-- the given show functions for requests and their results.
dataFetchWithShow
  :: (DataSource u r, Eq (r a), Hashable (r a), Typeable (r a))
  => ShowReq r a
  -> r a -> GenHaxl u w a
dataFetchWithShow (showReq, showRes) = dataFetchWithInsert showReq
  (DataCache.insertWithShow showReq showRes)

-- | Performs actual fetching of data for a 'Request' from a 'DataSource', using
-- the given function to insert requests in the cache.
dataFetchWithInsert
  :: forall u w r a
   . (DataSource u r, Eq (r a), Hashable (r a), Typeable (r a))
  => (r a -> String)    -- See Note [showFn]
  -> (r a -> IVar u w a -> DataCache (IVar u w) -> IO ())
  -> r a
  -> GenHaxl u w a
dataFetchWithInsert showFn insertFn req =
  GenHaxl $ \env@Env{..} -> do
  -- First, check the cache
  res <- cachedWithInsert showFn insertFn env req
  ifProfiling flags $ addProfileFetch env req
  case res of
    -- This request has not been seen before
    Uncached rvar ivar -> do
      logFetch env showFn req
      --
      -- Check whether the data source wants to submit requests
      -- eagerly, or batch them up.
      --
      case schedulerHint userEnv :: SchedulerHint r of
        SubmitImmediately -> do
          (_,ios) <- performFetches 0 env
            [BlockedFetches [BlockedFetch req rvar]]
          when (not (null ios)) $
            error "bad data source:SubmitImmediately but returns FutureFetch"
        TryToBatch ->
          -- add the request to the RequestStore and continue
          modifyIORef' reqStoreRef $ \bs ->
            addRequest (BlockedFetch req rvar) bs
      --
      return $ Blocked ivar (Return ivar)

    -- Seen before but not fetched yet.  We're blocked, but we don't have
    -- to add the request to the RequestStore.
    CachedNotFetched ivar -> return $ Blocked ivar (Return ivar)

    -- Cached: either a result, or an exception
    Cached r -> done r

-- | A data request that is not cached.  This is not what you want for
-- normal read requests, because then multiple identical requests may
-- return different results, and this invalidates some of the
-- properties that we expect Haxl computations to respect: that data
-- fetches can be arbitrarily reordered, and identical requests can be
-- commoned up, for example.
--
-- 'uncachedRequest' is useful for performing writes, provided those
-- are done in a safe way - that is, not mixed with reads that might
-- conflict in the same Haxl computation.
--
-- if we are recording or running a test, we fallback to using dataFetch
-- This allows us to store the request in the cache when recording, which
-- allows a transparent run afterwards. Without this, the test would try to
-- call the datasource during testing and that would be an exception.
uncachedRequest
 :: forall a u w (r :: * -> *). (DataSource u r, Request r a)
 => r a -> GenHaxl u w a
uncachedRequest req = do
  flg <- env flags
  subRef <- env submittedReqsRef
  if recording flg /= 0
    then dataFetch req
    else GenHaxl $ \Env{..} -> do
      ivar <- newIVar
      let !rvar = stdResultVar ivar completions subRef flg (Proxy :: Proxy r)
      modifyIORef' reqStoreRef $ \bs ->
        addRequest (BlockedFetch req rvar) bs
      return $ Blocked ivar (Return ivar)


-- | Transparently provides caching. Useful for datasources that can
-- return immediately, but also caches values.  Exceptions thrown by
-- the IO operation (except for asynchronous exceptions) are
-- propagated into the Haxl monad and can be caught by 'catch' and
-- 'try'.
cacheResult :: Request r a => r a -> IO a -> GenHaxl u w a
cacheResult = cacheResultWithInsert show DataCache.insert

-- | Transparently provides caching in the same way as 'cacheResult', but uses
-- the given functions to show requests and their results.
cacheResultWithShow
  :: (Eq (r a), Hashable (r a), Typeable (r a))
  => ShowReq r a -> r a -> IO a -> GenHaxl u w a
cacheResultWithShow (showReq, showRes) = cacheResultWithInsert showReq
  (DataCache.insertWithShow showReq showRes)

-- Transparently provides caching, using the given function to insert requests
-- into the cache.
cacheResultWithInsert
  :: Typeable (r a)
  => (r a -> String)    -- See Note [showFn]
  -> (r a -> IVar u w a -> DataCache (IVar u w) -> IO ()) -> r a
  -> IO a -> GenHaxl u w a
cacheResultWithInsert showFn insertFn req val = GenHaxl $ \Env{..} -> do
  mbRes <- DataCache.lookup req dataCache
  case mbRes of
    Nothing -> do
      eitherResult <- Exception.try val
      case eitherResult of
        Left e -> rethrowAsyncExceptions e
        _ -> return ()
      let result = eitherToResultThrowIO eitherResult
      ivar <- newFullIVar result
      insertFn req ivar dataCache
      done result
    Just (IVar cr) -> do
      e <- readIORef cr
      case e of
        IVarEmpty _ -> corruptCache
        IVarFull r -> done r
  where
    corruptCache = raise . DataSourceError $ Text.concat
      [ Text.pack (showFn req)
      , " has a corrupted cache value: these requests are meant to"
      , " return immediately without an intermediate value. Either"
      , " the cache was updated incorrectly, or you're calling"
      , " cacheResult on a query that involves a blocking fetch."
      ]

-- | Inserts a request/result pair into the cache. Throws an exception
-- if the request has already been issued, either via 'dataFetch' or
-- 'cacheRequest'.
--
-- This can be used to pre-populate the cache when running tests, to
-- avoid going to the actual data source and ensure that results are
-- deterministic.
--
cacheRequest
  :: Request req a => req a -> Either SomeException a -> GenHaxl u w ()
cacheRequest request result = GenHaxl $ \Env{..} -> do
  mbRes <- DataCache.lookup request dataCache
  case mbRes of
    Nothing -> do
      cr <- newFullIVar (eitherToResult result)
      DataCache.insert request cr dataCache
      return (Done ())

    -- It is an error if the request is already in the cache.
    -- We can't test whether the cached result is the same without adding an
    -- Eq constraint, and we don't necessarily have Eq for all results.
    _other -> raise $
      DataSourceError "cacheRequest: request is already in the cache"

-- | Similar to @cacheRequest@ but doesn't throw an exception if the key
-- already exists in the cache.
-- If this function is called twice to cache the same Haxl request, the first
-- value will be discarded and overwritten with the second value.
-- Useful e.g. for unit tests
dupableCacheRequest
  :: Request req a => req a -> Either SomeException a -> GenHaxl u w ()
dupableCacheRequest request result = GenHaxl $ \Env{..} -> do
  cr <- newFullIVar (eitherToResult result)
  DataCache.insert request cr dataCache
  return (Done ())

performRequestStore
   :: forall u w. Int -> Env u w -> RequestStore u -> IO (Int, [IO ()])
performRequestStore n env reqStore =
  performFetches n env (contents reqStore)

-- | Issues a batch of fetches in a 'RequestStore'. After
-- 'performFetches', all the requests in the 'RequestStore' are
-- complete, and all of the 'ResultVar's are full.
performFetches
  :: forall u w. Int -> Env u w -> [BlockedFetches u] -> IO (Int, [IO ()])
performFetches n env@Env{flags=f, statsRef=sref} jobs = do
  let !n' = n + length jobs

  t0 <- getTimestamp

  let
    roundstats =
      [ (dataSourceName (Proxy :: Proxy r), length reqs)
      | BlockedFetches (reqs :: [BlockedFetch r]) <- jobs ]

  ifTrace f 1 $
    printf "Batch data fetch (%s)\n" $
      intercalate (", "::String) $
        map (\(name,num) -> printf "%d %s" num (Text.unpack name)) roundstats

  ifTrace f 3 $
    forM_ jobs $ \(BlockedFetches reqs) ->
      forM_ reqs $ \(BlockedFetch r _) -> putStrLn (showp r)

  let
    applyFetch i (BlockedFetches (reqs :: [BlockedFetch r])) =
      case stateGet (states env) of
        Nothing ->
          return (FetchToDo reqs (SyncFetch (mapM_ (setError e))))
         where
           e :: ShowP req => req a -> DataSourceError
           e req = DataSourceError $ "data source not initialized: " <> dsName
                  <> ": "
                  <> Text.pack (showp req)
        Just state ->
          return
            $ FetchToDo reqs
            $ (if report f >= 2
                then wrapFetchInStats sref dsName (length reqs)
                else id)
            $ wrapFetchInTrace i (length reqs) dsName
            $ wrapFetchInCatch reqs
            $ fetch state f (userEnv env)
      where
        dsName = dataSourceName (Proxy :: Proxy r)

  fetches <- zipWithM applyFetch [n..] jobs

  waits <- scheduleFetches fetches (submittedReqsRef env) (flags env)

  t1 <- getTimestamp
  let roundtime = fromIntegral (t1 - t0) / 1000000 :: Double

  ifTrace f 1 $
    printf "Batch data fetch done (%.2fs)\n" (realToFrac roundtime :: Double)

  return (n', waits)

data FetchToDo where
  FetchToDo
    :: forall (req :: * -> *). (DataSourceName req, Typeable req)
    => [BlockedFetch req] -> PerformFetch req -> FetchToDo

-- Catch exceptions arising from the data source and stuff them into
-- the appropriate requests.  We don't want any exceptions propagating
-- directly from the data sources, because we want the exception to be
-- thrown by dataFetch instead.
--
wrapFetchInCatch :: [BlockedFetch req] -> PerformFetch req -> PerformFetch req
wrapFetchInCatch reqs fetch =
  case fetch of
    SyncFetch f ->
      SyncFetch $ \reqs -> f reqs `Exception.catch` handler
    AsyncFetch f ->
      AsyncFetch $ \reqs io -> f reqs io `Exception.catch` handler
      -- this might be wrong: if the outer 'fio' throws an exception,
      -- then we don't know whether we have executed the inner 'io' or
      -- not.  If not, then we'll likely get some errors about "did
      -- not set result var" later, because we haven't executed some
      -- data fetches.  But we can't execute 'io' in the handler,
      -- because we might have already done it.  It isn't possible to
      -- do it completely right here, so we have to rely on data
      -- sources themselves to catch (synchronous) exceptions.  Async
      -- exceptions aren't a problem because we're going to rethrow
      -- them all the way to runHaxl anyway.
    FutureFetch f ->
      FutureFetch $ \reqs -> f reqs `Exception.catch` (
                     \e -> handler e >> return (return ()))
    BackgroundFetch f ->
      BackgroundFetch $ \reqs -> f reqs `Exception.catch` handler
  where
    handler :: SomeException -> IO ()
    handler e = do
      rethrowAsyncExceptions e
      mapM_ (forceError e) reqs

    -- Set the exception even if the request already had a result.
    -- Otherwise we could be discarding an exception.
    forceError e (BlockedFetch _ rvar) =
      putResult rvar (except e)


wrapFetchInStats
  :: IORef Stats
  -> Text
  -> Int
  -> PerformFetch req
  -> PerformFetch req

wrapFetchInStats !statsRef dataSource batchSize perform = do
  case perform of
    SyncFetch f ->
      SyncFetch $ \reqs -> do
        fail_ref <- newIORef 0
        (t0,t,alloc,_) <- statsForIO (f (map (addFailureCount fail_ref) reqs))
        failures <- readIORef fail_ref
        updateFetchStats t0 t alloc batchSize failures
    AsyncFetch f -> do
      AsyncFetch $ \reqs inner -> do
        inner_r <- newIORef (0, 0)
        fail_ref <- newIORef 0
        let inner' = do
              (_,t,alloc,_) <- statsForIO inner
              writeIORef inner_r (t,alloc)
            reqs' = map (addFailureCount fail_ref) reqs
        (t0, totalTime, totalAlloc, _) <- statsForIO (f reqs' inner')
        (innerTime, innerAlloc) <- readIORef inner_r
        failures <- readIORef fail_ref
        updateFetchStats t0 (totalTime - innerTime) (totalAlloc - innerAlloc)
          batchSize failures
    FutureFetch submit ->
      FutureFetch $ \reqs -> do
        fail_ref <- newIORef 0
        let reqs' = map (addFailureCount fail_ref) reqs
        (t0, submitTime, submitAlloc, wait) <- statsForIO (submit reqs')
        return $ do
          (_, waitTime, waitAlloc, _) <- statsForIO wait
          failures <- readIORef fail_ref
          updateFetchStats t0 (submitTime + waitTime) (submitAlloc + waitAlloc)
            batchSize failures
    BackgroundFetch io -> do
      BackgroundFetch $ \reqs -> do
        startTime <- getTimestamp
        io (map (addTimer startTime) reqs)
  where
    statsForIO io = do
      prevAlloc <- getAllocationCounter
      (t0,t,a) <- time io
      postAlloc <- getAllocationCounter
      return (t0,t, fromIntegral $ prevAlloc - postAlloc, a)

    addTimer t0 (BlockedFetch req (ResultVar fn)) =
      BlockedFetch req $ ResultVar $ \result isChildThread -> do
        t1 <- getTimestamp
        -- We cannot measure allocation easily for BackgroundFetch. Here we
        -- just attribute all allocation to the last `putResultFromChildThread`
        -- and use 0 for the others. While the individual allocations may not
        -- be correct, the total sum and amortized allocation are still
        -- meaningful.
        -- see Note [tracking allocation in child threads]
        allocs <- if isChildThread then getAllocationCounter else return 0
        updateFetchStats t0 (t1 - t0)
          (negate allocs)
          1 -- batch size: we don't know if this is a batch or not
          (if isLeft result then 1 else 0) -- failures
        fn result isChildThread

    updateFetchStats
      :: Timestamp -> Microseconds -> Int64 -> Int -> Int -> IO ()
    updateFetchStats start time space batch failures = do
      let this = FetchStats { fetchDataSource = dataSource
                            , fetchBatchSize = batch
                            , fetchStart = start
                            , fetchDuration = time
                            , fetchSpace = space
                            , fetchFailures = failures }
      atomicModifyIORef' statsRef $ \(Stats fs) -> (Stats (this : fs), ())

    addFailureCount :: IORef Int -> BlockedFetch r -> BlockedFetch r
    addFailureCount ref (BlockedFetch req (ResultVar fn)) =
      BlockedFetch req $ ResultVar $ \result isChildThread -> do
        when (isLeft result) $ atomicModifyIORef' ref (\r -> (r+1,()))
        fn result isChildThread

wrapFetchInTrace
  :: Int
  -> Int
  -> Text
  -> PerformFetch req
  -> PerformFetch req
#ifdef EVENTLOG
wrapFetchInTrace i n dsName f =
  case f of
    SyncFetch io -> SyncFetch (wrapF "Sync" io)
    AsyncFetch fio -> AsyncFetch (wrapF "Async" . fio . unwrapF "Async")
  where
    d = Text.unpack dsName
    wrapF :: String -> IO a -> IO a
    wrapF ty = bracket_ (traceEventIO $ printf "START %d %s (%d %s)" i d n ty)
                        (traceEventIO $ printf "STOP %d %s (%d %s)" i d n ty)
    unwrapF :: String -> IO a -> IO a
    unwrapF ty = bracket_ (traceEventIO $ printf "STOP %d %s (%d %s)" i d n ty)
                          (traceEventIO $ printf "START %d %s (%d %s)" i d n ty)
#else
wrapFetchInTrace _ _ _ f = f
#endif

time :: IO a -> IO (Timestamp,Microseconds,a)
time io = do
  t0 <- getTimestamp
  a <- io
  t1 <- getTimestamp
  return (t0, t1 - t0, a)

-- | Start all the async fetches first, then perform the sync fetches before
-- getting the results of the async fetches.
scheduleFetches :: [FetchToDo] -> IORef ReqCountMap -> Flags -> IO [IO ()]
scheduleFetches fetches ref flags = do
  -- update ReqCountmap for these fetches
  ifReport flags 1 $ sequence_
    [ atomicModifyIORef' ref $
        \m -> (addToCountMap (Proxy :: Proxy r) (length reqs) m, ())
    | FetchToDo (reqs :: [BlockedFetch r]) _f <- fetches
    ]
  fully_async_fetches
  waits <- future_fetches
  async_fetches sync_fetches
  return waits
 where
  fully_async_fetches :: IO ()
  fully_async_fetches = sequence_
    [f reqs | FetchToDo reqs (BackgroundFetch f) <- fetches]

  future_fetches :: IO [IO ()]
  future_fetches = sequence
    [f reqs | FetchToDo reqs (FutureFetch f) <- fetches]

  async_fetches :: IO () -> IO ()
  async_fetches = compose
    [f reqs | FetchToDo reqs (AsyncFetch f) <- fetches]

  sync_fetches :: IO ()
  sync_fetches = sequence_
    [f reqs | FetchToDo reqs (SyncFetch f) <- fetches]


-- | An exception thrown when reading from datasources fails
data ReadingCompletionsFailedFetch = ReadingCompletionsFailedFetch Text
  deriving Show

instance Exception ReadingCompletionsFailedFetch
