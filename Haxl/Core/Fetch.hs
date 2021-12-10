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
       {-# UNPACK #-} !CallId

  -- | The request has been seen before, but its result has not yet been
  -- fetched.
  | CachedNotFetched
      {-# UNPACK #-} !(IVar u w a)
       {-# UNPACK #-} !CallId

  -- | The request has been seen before, and its result has already been
  -- fetched.
  | Cached (ResultVal a w)
           {-# UNPACK #-} !CallId


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
  -> (r a -> DataCacheItem u w a -> DataCache (DataCacheItem u w) -> IO ())
  -> Env u w -> r a -> IO (CacheResult u w a)
cachedWithInsert showFn insertFn env@Env{..} req = do
  let
    doFetch = do
      ivar <- newIVar
      k <- nextCallId env
      let !rvar = stdResultVar env ivar (Proxy :: Proxy r)
      insertFn req (DataCacheItem ivar k) dataCache
      return (Uncached rvar ivar k)
  mbRes <- DataCache.lookup req dataCache
  case mbRes of
    Nothing -> doFetch
    Just (DataCacheItem i@IVar{ivarRef = cr} k) -> do
      e <- readIORef cr
      case e of
        IVarEmpty _ -> do
          ivar <- withCurrentCCS i
          return (CachedNotFetched ivar k)
        IVarFull r -> do
          ifTrace flags 3 $ putStrLn $ case r of
            ThrowIO{} -> "Cached error: " ++ showFn req
            ThrowHaxl{} -> "Cached error: " ++ showFn req
            Ok{} -> "Cached request: " ++ showFn req
          return (Cached r k)


-- | Make a ResultVar with the standard function for sending a CompletionReq
-- to the scheduler. This is the function will be executed when the fetch
-- completes.
stdResultVar
  :: forall r a u w. (DataSourceName r, Typeable r)
  => Env u w
  -> IVar u w a
  -> Proxy r
  -> ResultVar a
stdResultVar Env{..} ivar p =
  mkResultVar $ \r isChildThread _ -> do
    allocs <- if isChildThread
      then
        -- In a child thread, return the current allocation counter too,
        -- for correct tracking of allocation.
        getAllocationCounter
      else
        return 0
    atomicallyOnBlocking
      (LogicBug (ReadingCompletionsFailedFetch (dataSourceName p))) $ do
      cs <- readTVar completions
      writeTVar completions (CompleteReq (eitherToResult r) ivar allocs : cs)
    -- Decrement the counter as request has finished. Do this after updating the
    -- completions TVar so that if the scheduler is tracking what was being
    -- waited on it gets a consistent view.
    ifReport flags ReportOutgoneFetches $
      atomicModifyIORef' submittedReqsRef (\m -> (subFromCountMap p 1 m, ()))
{-# INLINE stdResultVar #-}


-- | Record the call stack for a data fetch in the Stats.  Only useful
-- when profiling.
logFetch :: Env u w -> (r a -> String) -> r a -> CallId -> IO ()
#ifdef PROFILING
logFetch env showFn req fid = do
  ifReport (flags env) ReportFetchStack $ do
    stack <- currentCallStack
    modifyIORef' (statsRef env) $ \(Stats s) ->
      Stats (FetchCall (showFn req) stack fid : s)
#else
logFetch _ _ _ _ = return ()
#endif

calcFailure
  :: forall u req a . DataSource u req
  => u
  -> req a
  -> Either SomeException a
  -> FailureCount
calcFailure _u _r Right{} = mempty
calcFailure u r (Left e) = case classifyFailure u r e of
  StandardFailure -> mempty { failureCountStandard = 1 }
  IgnoredForStatsFailure -> mempty { failureCountIgnored = 1 }

addFallbackFetchStats
  :: forall u w req a . DataSource u req
  => Env u w
  -> CallId
  -> req a
  -> ResultVal a w
  -> IO ()
addFallbackFetchStats Env{..} fid req res = do
  bid <- atomicModifyIORef' statsBatchIdRef $ \x -> (x+1,x+1)
  start <- getTimestamp
  let
    dsName = dataSourceName (Proxy :: Proxy req)
    FailureCount{..} = case res of
      Ok{} -> mempty
      (ThrowHaxl e _) -> calcFailure userEnv req (Left e)
      (ThrowIO e) -> calcFailure userEnv req (Left e)
    this = FetchStats { fetchDataSource = dsName
                      , fetchBatchSize = 1
                      , fetchStart = start
                      , fetchDuration = 0
                      , fetchSpace = 0
                      , fetchFailures = failureCountStandard
                      , fetchIgnoredFailures = failureCountIgnored
                      , fetchBatchId = bid
                      , fetchIds = [fid] }
  atomicModifyIORef' statsRef $ \(Stats fs) -> (Stats (this : fs), ())

addFallbackResult
  :: Env u w
  -> ResultVal a w
  -> IVar u w a
  -> IO ()
addFallbackResult Env{..} res ivar = do
  atomicallyOnBlocking
    (LogicBug (ReadingCompletionsFailedFetch "addFallbackResult")) $ do
    cs <- readTVar completions
    writeTVar completions (CompleteReq res ivar 0 : cs)

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
  -> (r a -> DataCacheItem u w a -> DataCache (DataCacheItem u w) -> IO ())
  -> r a
  -> GenHaxl u w a
dataFetchWithInsert showFn insertFn req =
  GenHaxl $ \env@Env{..} -> do
  -- First, check the cache
  res <- cachedWithInsert showFn insertFn env req
  case res of
    -- This request has not been seen before
    Uncached rvar ivar fid -> do
      logFetch env showFn req fid
      ifProfiling flags $ addProfileFetch env req fid False
      --
      -- Check whether the data source wants to submit requests
      -- eagerly, or batch them up.
      --
      let
        blockedFetch = BlockedFetch req rvar
        blockedFetchI = BlockedFetchInternal fid
        submitFetch = do
          case schedulerHint userEnv :: SchedulerHint r of
            SubmitImmediately ->
              performFetches env [BlockedFetches [blockedFetch] [blockedFetchI]]
            TryToBatch ->
              -- add the request to the RequestStore and continue
              modifyIORef' reqStoreRef $ \bs ->
                addRequest blockedFetch blockedFetchI bs
          return $ Blocked ivar (Return ivar)

      -- if there is a fallback configured try that,
      -- else dispatch the fetch
      case dataCacheFetchFallback of
        Nothing -> submitFetch
        Just (DataCacheLookup dcl) -> do
          mbFallbackRes <- dcl req
          case mbFallbackRes of
            Nothing -> submitFetch
            Just fallbackRes -> do
              addFallbackResult env fallbackRes ivar
              ifReport flags ReportFetchStats $ addFallbackFetchStats
                env
                fid
                req
                fallbackRes
              return $ Blocked ivar (Return ivar)

    -- Seen before but not fetched yet.  We're blocked, but we don't have
    -- to add the request to the RequestStore.
    CachedNotFetched ivar fid -> do
      ifProfiling flags $ addProfileFetch env req fid True
      return $ Blocked ivar (Return ivar)

    -- Cached: either a result, or an exception
    Cached r fid -> do
      ifProfiling flags $ addProfileFetch env req fid True
      done r

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
  if recording flg /= 0
    then dataFetch req
    else GenHaxl $ \e@Env{..} -> do
      ivar <- newIVar
      k <- nextCallId e
      let !rvar = stdResultVar e ivar (Proxy :: Proxy r)
      modifyIORef' reqStoreRef $ \bs ->
        addRequest (BlockedFetch req rvar) (BlockedFetchInternal k) bs
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
  -> (r a -> DataCacheItem u w a -> DataCache (DataCacheItem u w) -> IO ())
  -> r a
  -> IO a
  -> GenHaxl u w a
cacheResultWithInsert showFn insertFn req val = GenHaxl $ \e@Env{..} -> do
  mbRes <- DataCache.lookup req dataCache
  case mbRes of
    Nothing -> do
      let
        getResult = do
          eitherResult <- Exception.try val
          case eitherResult of
            Left e -> rethrowAsyncExceptions e
            _ -> return ()
          return $ eitherToResultThrowIO eitherResult
      -- if there is a fallback configured try that
      result <- case dataCacheFetchFallback of
        Nothing -> getResult
        Just (DataCacheLookup dcl) -> do
          mbFallbackRes <- dcl req
          maybe getResult return mbFallbackRes
      ivar <- newFullIVar result
      k <- nextCallId e
      insertFn req (DataCacheItem ivar k) dataCache
      done result
    Just (DataCacheItem IVar{ivarRef = cr} _) -> do
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
cacheRequest request result = GenHaxl $ \e@Env{..} -> do
  mbRes <- DataCache.lookup request dataCache
  case mbRes of
    Nothing -> do
      cr <- newFullIVar (eitherToResult result)
      k <- nextCallId e
      DataCache.insert request (DataCacheItem cr k) dataCache
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
dupableCacheRequest request result = GenHaxl $ \e@Env{..} -> do
  cr <- newFullIVar (eitherToResult result)
  k <- nextCallId e
  DataCache.insert request (DataCacheItem cr k) dataCache
  return (Done ())

performRequestStore
   :: forall u w. Env u w -> RequestStore u -> IO ()
performRequestStore env reqStore =
  performFetches env (contents reqStore)

-- | Issues a batch of fetches in a 'RequestStore'. After
-- 'performFetches', all the requests in the 'RequestStore' are
-- complete, and all of the 'ResultVar's are full.
performFetches
  :: forall u w. Env u w -> [BlockedFetches u] -> IO ()
performFetches env@Env{flags=f, statsRef=sref, statsBatchIdRef=sbref} jobs = do
  t0 <- getTimestamp

  ifTrace f 3 $
    forM_ jobs $ \(BlockedFetches reqs _) ->
      forM_ reqs $ \(BlockedFetch r _) -> putStrLn (showp r)

  let
    applyFetch i bfs@(BlockedFetches (reqs :: [BlockedFetch r]) _) =
      case stateGet (states env) of
        Nothing ->
          return (FetchToDo reqs (SyncFetch (mapM_ (setError e))))
         where
           e :: ShowP req => req a -> DataSourceError
           e req = DataSourceError $ "data source not initialized: " <> dsName
                  <> ": "
                  <> Text.pack (showp req)
        Just state ->
          return $ FetchToDo reqs
            $ (if testReportFlag ReportFetchStats $ report f
                then wrapFetchInStats
                        (userEnv env)
                        sref
                        sbref
                        dsName
                        (length reqs)
                        bfs
                else id)
            $ wrapFetchInTrace i (length reqs) dsName
            $ wrapFetchInCatch reqs
            $ fetch state f (userEnv env)
      where
        dsName = dataSourceName (Proxy :: Proxy r)

  fetches <- zipWithM applyFetch [0..] jobs

  scheduleFetches fetches (submittedReqsRef env) (flags env)

  t1 <- getTimestamp
  let roundtime = fromIntegral (t1 - t0) / 1000000 :: Double

  ifTrace f 1 $
    printf "Batch data fetch done (%.4fs)\n" (realToFrac roundtime :: Double)

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


data FailureCount = FailureCount
  { failureCountStandard :: {-# UNPACK #-} !Int
  , failureCountIgnored :: {-# UNPACK #-} !Int
  }

#if __GLASGOW_HASKELL__ >= 804
instance Semigroup FailureCount where
  (<>) = mappend
#endif

instance Monoid FailureCount where
  mempty = FailureCount 0 0
  mappend (FailureCount s1 i1) (FailureCount s2 i2)
    = FailureCount (s1+s2) (i1+i2)

wrapFetchInStats
  :: DataSource u req
  => u
  -> IORef Stats
  -> IORef Int
  -> Text
  -> Int
  -> BlockedFetches u
  -> PerformFetch req
  -> PerformFetch req
wrapFetchInStats
  u
  !statsRef
  !batchIdRef
  dataSource
  batchSize
  (BlockedFetches _reqs reqsI)
  perform = do
  case perform of
    SyncFetch f ->
      SyncFetch $ \reqs -> do
        bid <- newBatchId
        fail_ref <- newIORef mempty
        (t0,t,alloc,_) <- statsForIO (f (map (addFailureCount u fail_ref)
          (reqsWithFetchDsStats bid reqs)))
        failures <- readIORef fail_ref
        updateFetchStats bid allFids t0 t alloc batchSize failures
    AsyncFetch f -> do
      AsyncFetch $ \reqs inner -> do
        bid <- newBatchId
        inner_r <- newIORef (0, 0)
        fail_ref <- newIORef mempty
        let inner' = do
              (_,t,alloc,_) <- statsForIO inner
              writeIORef inner_r (t,alloc)
            reqs' = map (addFailureCount u fail_ref) reqs
            reqs'' = reqsWithFetchDsStats bid reqs'
        (t0, totalTime, totalAlloc, _) <- statsForIO (f reqs'' inner')
        (innerTime, innerAlloc) <- readIORef inner_r
        failures <- readIORef fail_ref
        updateFetchStats bid allFids t0 (totalTime - innerTime)
          (totalAlloc - innerAlloc) batchSize failures
    BackgroundFetch io -> do
      BackgroundFetch $ \reqs -> do
        bid <- newBatchId
        startTime <- getTimestamp
        io (reqsWithFetchDsStats bid
          (zipWith (addTimer u bid startTime) reqs reqsI))
  where
    allFids = map (\(BlockedFetchInternal k) -> k) reqsI
    newBatchId = atomicModifyIORef' batchIdRef $ \x -> (x+1,x+1)
    statsForIO io = do
      prevAlloc <- getAllocationCounter
      (t0,t,a) <- time io
      postAlloc <- getAllocationCounter
      return (t0,t, fromIntegral $ prevAlloc - postAlloc, a)
    reqsWithFetchDsStats = \bid reqs
      -> zipWith (addFetchDatasourceStats bid) reqs reqsI
    addTimer
      u
      bid
      t0
      (BlockedFetch req (ResultVar fn))
      (BlockedFetchInternal fid) =
        BlockedFetch req $ ResultVar $ \result isChildThread stats -> do
          t1 <- getTimestamp
          -- We cannot measure allocation easily for BackgroundFetch. Here we
          -- just attribute all allocation to the last
          -- `putResultFromChildThread` and use 0 for the others.
          -- While the individual allocations may not be correct,
          -- the total sum and amortized allocation are still meaningful.
          -- see Note [tracking allocation in child threads]
          allocs <- if isChildThread then getAllocationCounter else return 0
          updateFetchStats bid [fid] t0 (t1 - t0)
            (negate allocs)
            1 -- batch size: we don't know if this is a batch or not
            (calcFailure u req result) -- failures
          fn result isChildThread stats

    addFetchDatasourceStats
      :: Int
      -> BlockedFetch r
      -> BlockedFetchInternal
      -> BlockedFetch r
    addFetchDatasourceStats bid
      (BlockedFetch req (ResultVar fn))
      (BlockedFetchInternal fid) = BlockedFetch req $ ResultVar
        $ \result isChildThread stats -> do
          let mkStats dss = FetchDataSourceStats
                { fetchDsStatsCallId = fid
                , fetchDsStatsDataSource = dataSource
                , fetchDsStatsStats = dss
                , fetchBatchId = bid
                }
          case stats of
            Just dss -> atomicModifyIORef' statsRef
              $ \(Stats fs) -> (Stats (mkStats dss : fs), ())
            Nothing -> return ()
          fn result isChildThread stats


    updateFetchStats
      :: Int
      -> [CallId]
      -> Timestamp
      -> Microseconds
      -> Int64
      -> Int
      -> FailureCount
      -> IO ()
    updateFetchStats bid fids start time space batch FailureCount{..} = do
      let this = FetchStats { fetchDataSource = dataSource
                            , fetchBatchSize = batch
                            , fetchStart = start
                            , fetchDuration = time
                            , fetchSpace = space
                            , fetchFailures = failureCountStandard
                            , fetchIgnoredFailures = failureCountIgnored
                            , fetchBatchId = bid
                            , fetchIds = fids }
      atomicModifyIORef' statsRef $ \(Stats fs) -> (Stats (this : fs), ())

    addFailureCount :: DataSource u r
      => u -> IORef FailureCount -> BlockedFetch r -> BlockedFetch r
    addFailureCount u ref (BlockedFetch req (ResultVar fn)) =
      BlockedFetch req $ ResultVar $ \result isChildThread stats -> do
        let addFailures r = (r <> calcFailure u req result, ())
        when (isLeft result) $ atomicModifyIORef' ref addFailures
        fn result isChildThread stats

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
scheduleFetches :: [FetchToDo] -> IORef ReqCountMap -> Flags -> IO ()
scheduleFetches fetches ref flags = do
  -- update ReqCountmap for these fetches
  ifReport flags ReportOutgoneFetches $ sequence_
    [ atomicModifyIORef' ref $
        \m -> (addToCountMap (Proxy :: Proxy r) (length reqs) m, ())
    | FetchToDo (reqs :: [BlockedFetch r]) _f <- fetches
    ]

  ifTrace flags 1 $ printf "Batch data fetch round: %s\n" $
     intercalate (", "::String) $
        map (\(c, n, ds) -> printf "%s %s %d" n ds c) stats

  fully_async_fetches
  async_fetches sync_fetches
 where

  fetchName :: forall req . PerformFetch req -> String
  fetchName (BackgroundFetch _) = "background"
  fetchName (AsyncFetch _) = "async"
  fetchName (SyncFetch _) = "sync"

  srcName :: forall req . (DataSourceName req) => [BlockedFetch req] -> String
  srcName _ = Text.unpack $ dataSourceName (Proxy :: Proxy req)

  stats = [(length reqs, fetchName f, srcName reqs)
          | FetchToDo reqs f <- fetches]

  fully_async_fetches :: IO ()
  fully_async_fetches = sequence_
    [f reqs | FetchToDo reqs (BackgroundFetch f) <- fetches]

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
