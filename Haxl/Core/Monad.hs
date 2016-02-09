-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file. An additional grant of patent rights can
-- be found in the PATENTS file.

{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The implementation of the 'Haxl' monad.  Most users should
-- import "Haxl.Core" instead of importing this module directly.
module Haxl.Core.Monad (
    -- * The monad
    GenHaxl (..), runHaxl,
    env, withEnv,

    -- * Env
    Env(..), Caches, caches, initEnvWithData, initEnv, emptyEnv,

    -- * Exceptions
    throw, catch, catchIf, try, tryToHaxlException,

    -- * Data fetching and caching
    dataFetch, uncachedRequest,
    cacheRequest, cacheResult, cachedComputation,
    dumpCacheAsHaskell,

    -- * Unsafe operations
    unsafeLiftIO, unsafeToHaxlException,
  ) where

import Haxl.Core.Types
import Haxl.Core.Show1
import Haxl.Core.StateStore
import Haxl.Core.Exception
import Haxl.Core.RequestStore
import Haxl.Core.Util
import Haxl.Core.DataCache as DataCache

import qualified Data.Text as Text
import qualified Control.Monad.Catch as Catch
import Control.Exception (Exception(..), SomeException)
#if __GLASGOW_HASKELL__ >= 708
import Control.Exception (SomeAsyncException(..))
#endif
#if __GLASGOW_HASKELL__ >= 710
import Control.Exception (AllocationLimitExceeded(..))
#endif
import Control.Monad
import qualified Control.Exception as Exception
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative hiding (Const)
#endif
import Control.DeepSeq
import GHC.Exts (IsString(..))
#if __GLASGOW_HASKELL__ < 706
import Prelude hiding (catch)
#endif
import Data.Hashable
import Data.IORef
import Data.List
import Data.Monoid
import Data.Time
import Data.Typeable
import qualified Data.HashMap.Strict as HashMap
import Text.Printf
import Text.PrettyPrint hiding ((<>))
import Control.Arrow (left)
#ifdef EVENTLOG
import Control.Exception (bracket_)
import Debug.Trace (traceEventIO)
#endif

-- -----------------------------------------------------------------------------
-- The environment

-- | The data we carry around in the Haxl monad.
data Env u = Env
  { cacheRef     :: IORef (DataCache ResultVar) -- cached data fetches
  , memoRef      :: IORef (DataCache (MemoVar u))   -- memoized computations
  , flags        :: Flags
  , userEnv      :: u
  , statsRef     :: IORef Stats
  , states       :: StateStore
  -- ^ Data sources and other components can store their state in
  -- here. Items in this store must be instances of 'StateKey'.
  }

type Caches u = (IORef (DataCache ResultVar), IORef (DataCache (MemoVar u)))

caches :: Env u -> Caches u
caches env = (cacheRef env, memoRef env)

-- | Initialize an environment with a 'StateStore', an input map, a
-- preexisting 'DataCache', and a seed for the random number generator.
initEnvWithData :: StateStore -> u -> Caches u -> IO (Env u)
initEnvWithData states e (cref, mref) = do
  sref <- newIORef emptyStats
  return Env
    { cacheRef = cref
    , memoRef = mref
    , flags = defaultFlags
    , userEnv = e
    , states = states
    , statsRef = sref
    }

-- | Initializes an environment with 'StateStore' and an input map.
initEnv :: StateStore -> u -> IO (Env u)
initEnv states e = do
  cref <- newIORef DataCache.empty
  mref <- newIORef DataCache.empty
  initEnvWithData states e (cref,mref)

-- | A new, empty environment.
emptyEnv :: u -> IO (Env u)
emptyEnv = initEnv stateEmpty

-- -----------------------------------------------------------------------------
-- | The Haxl monad, which does several things:
--
--  * It is a reader monad for 'Env' and 'IORef' 'RequestStore', The
--    latter is the current batch of unsubmitted data fetch requests.
--
--  * It is a concurrency, or resumption, monad. A computation may run
--    partially and return 'Blocked', in which case the framework should
--    perform the outstanding requests in the 'RequestStore', and then
--    resume the computation.
--
--  * The Applicative combinator '<*>' explores /both/ branches in the
--    event that the left branch is 'Blocked', so that we can collect
--    multiple requests and submit them as a batch.
--
--  * It contains IO, so that we can perform real data fetching.
--
newtype GenHaxl u a = GenHaxl
  { unHaxl :: Env u -> IORef (RequestStore u) -> IO (Result u a) }

-- | The result of a computation is either 'Done' with a value, 'Throw'
-- with an exception, or 'Blocked' on the result of a data fetch with
-- a continuation.
data Result u a
  = Done a
  | Throw SomeException
  | Blocked (Cont u a)

data Cont u a
  = Cont (GenHaxl u a)
  | forall b. Cont u b :>>= (b -> GenHaxl u a)
  | forall b. (Cont u (b -> a)) :<*> (Cont u b)
  | forall b. (b -> a) :<$> (Cont u b)

toHaxl :: Cont u a -> GenHaxl u a
toHaxl (Cont haxl)           = haxl
toHaxl ((m :>>= k1) :>>= k2) = toHaxl (m :>>= (k1 >=> k2)) -- for seql
toHaxl (c :>>= k)            = toHaxl c >>= k
toHaxl ((f :<$> i) :<*> (g :<$> j)) =
  toHaxl (((\x y -> f x (g y)) :<$> i) :<*> j)          -- See Note [Tree]
toHaxl (f :<*> x)            = toHaxl f <*> toHaxl x
toHaxl (f :<$> (g :<$> x))   = toHaxl ((f . g) :<$> x)  -- fmap fusion
toHaxl (f :<$> x)            = fmap f (toHaxl x)

-- Note [Tree]
-- This implements the following re-association:
--
--           <*>
--          /   \
--       <$>     <$>
--      /   \   /   \
--     f     i g     j
--
-- to:
--
--           <*>
--          /   \
--       <$>     j
--      /   \         where h = (\x y -> f x (g y))
--     h     i
--
-- I suspect this is mostly useful because it eliminates one :<$> constructor
-- within the Blocked returned by `tree 1`, which is replicated a lot by the
-- tree benchmark (tree 1 is near the leaves). So this rule might just be
-- optimizing for a microbenchmark.

instance (Show a) => Show (Result u a) where
  show (Done a) = printf "Done(%s)" $ show a
  show (Throw e) = printf "Throw(%s)" $ show e
  show Blocked{} = "Blocked"

instance Monad (GenHaxl u) where
  return a = GenHaxl $ \_env _ref -> return (Done a)
  GenHaxl m >>= k = GenHaxl $ \env ref -> do
    e <- m env ref
    case e of
      Done a       -> unHaxl (k a) env ref
      Throw e      -> return (Throw e)
      Blocked cont -> return (Blocked (cont :>>= k))

  -- We really want the Applicative version of >>
  (>>) = (*>)

instance Functor (GenHaxl u) where
  fmap f (GenHaxl m) = GenHaxl $ \env ref -> do
    r <- m env ref
    case r of
      Done a -> return (Done (f a))
      Throw e -> return (Throw e)
      Blocked a' -> return (Blocked (f :<$> a'))

instance Applicative (GenHaxl u) where
  pure = return
  GenHaxl f <*> GenHaxl a = GenHaxl $ \env ref -> do
    r <- f env ref
    case r of
      Throw e -> return (Throw e)
      Done f' -> do
        ra <- a env ref
        case ra of
          Done a'    -> return (Done (f' a'))
          Throw e    -> return (Throw e)
          Blocked a' -> return (Blocked (f' :<$> a'))
      Blocked f' -> do
        ra <- a env ref  -- left is blocked, explore the right
        case ra of
          Done a'    -> return (Blocked (($ a') :<$> f'))
          Throw e    -> return (Blocked (f' :<*> Cont (throw e)))
          Blocked a' -> return (Blocked (f' :<*> a'))

-- | Runs a 'Haxl' computation in an 'Env'.
runHaxl :: Env u -> GenHaxl u a -> IO a
#ifdef EVENTLOG
runHaxl env h = do
  let go !n env c = do
        traceEventIO "START computation"
        ref <- newIORef noRequests
        e <- toHaxl c env ref
        traceEventIO "STOP computation"
        case e of
          Done a       -> return a
          Throw e      -> Exception.throw e
          Blocked cont -> do
            bs <- readIORef ref
            writeIORef ref noRequests -- Note [RoundId]
            traceEventIO "START performFetches"
            n' <- performFetches n env bs
            traceEventIO "STOP performFetches"
            go n' env cont
  traceEventIO "START runHaxl"
  r <- go 0 env (Cont h)
  traceEventIO "STOP runHaxl"
  return r
#else
runHaxl env (GenHaxl haxl) = do
  ref <- newIORef noRequests
  e <- haxl env ref
  case e of
    Done a       -> return a
    Throw e      -> Exception.throw e
    Blocked cont -> do
      bs <- readIORef ref
      writeIORef ref noRequests -- Note [RoundId]
      void (performFetches 0 env bs)
      runHaxl env (toHaxl cont)
#endif

-- | Extracts data from the 'Env'.
env :: (Env u -> a) -> GenHaxl u a
env f = GenHaxl $ \env _ref -> return (Done (f env))

-- | Returns a version of the Haxl computation which always uses the
-- provided 'Env', ignoring the one specified by 'runHaxl'.
withEnv :: Env u -> GenHaxl u a -> GenHaxl u a
withEnv newEnv (GenHaxl m) = GenHaxl $ \_env ref -> do
  r <- m newEnv ref
  case r of
    Done a -> return (Done a)
    Throw e -> return (Throw e)
    Blocked k -> return (Blocked (Cont (withEnv newEnv (toHaxl k))))

-- -----------------------------------------------------------------------------
-- Exceptions

-- | Throw an exception in the Haxl monad
throw :: (Exception e) => e -> GenHaxl u a
throw e = GenHaxl $ \_env _ref -> raise e

raise :: (Exception e) => e -> IO (Result u a)
raise = return . Throw . toException

-- | Catch an exception in the Haxl monad
catch :: Exception e => GenHaxl u a -> (e -> GenHaxl u a) -> GenHaxl u a
catch (GenHaxl m) h = GenHaxl $ \env ref -> do
   r <- m env ref
   case r of
     Done a    -> return (Done a)
     Throw e | Just e' <- fromException e -> unHaxl (h e') env ref
             | otherwise -> return (Throw e)
     Blocked k -> return (Blocked (Cont (catch (toHaxl k) h)))

-- | Catch exceptions that satisfy a predicate
catchIf
  :: Exception e => (e -> Bool) -> GenHaxl u a -> (e -> GenHaxl u a)
  -> GenHaxl u a
catchIf cond haxl handler =
  catch haxl $ \e -> if cond e then handler e else throw e

-- | Returns @'Left' e@ if the computation throws an exception @e@, or
-- @'Right' a@ if it returns a result @a@.
try :: Exception e => GenHaxl u a -> GenHaxl u (Either e a)
try haxl = (Right <$> haxl) `catch` (return . Left)

-- | @since 0.3.1.0
instance Catch.MonadThrow (GenHaxl u) where throwM = Haxl.Core.Monad.throw
-- | @since 0.3.1.0
instance Catch.MonadCatch (GenHaxl u) where catch = Haxl.Core.Monad.catch

-- -----------------------------------------------------------------------------
-- Unsafe operations

-- | Under ordinary circumstances this is unnecessary; users of the Haxl
-- monad should generally /not/ perform arbitrary IO.
unsafeLiftIO :: IO a -> GenHaxl u a
unsafeLiftIO m = GenHaxl $ \_env _ref -> Done <$> m

-- | Convert exceptions in the underlying IO monad to exceptions in
-- the Haxl monad.  This is morally unsafe, because you could then
-- catch those exceptions in Haxl and observe the underlying execution
-- order.  Not to be exposed to user code.
unsafeToHaxlException :: GenHaxl u a -> GenHaxl u a
unsafeToHaxlException (GenHaxl m) = GenHaxl $ \env ref -> do
  r <- m env ref `Exception.catch` \e -> return (Throw e)
  case r of
    Blocked c -> return (Blocked (Cont (unsafeToHaxlException (toHaxl c))))
    other -> return other

-- | Like 'try', but lifts all exceptions into the 'HaxlException'
-- hierarchy.  Uses 'unsafeToHaxlException' internally.  Typically
-- this is used at the top level of a Haxl computation, to ensure that
-- all exceptions are caught.
tryToHaxlException :: GenHaxl u a -> GenHaxl u (Either HaxlException a)
tryToHaxlException h = left asHaxlException <$> try (unsafeToHaxlException h)


-- -----------------------------------------------------------------------------
-- Data fetching and caching

-- | Possible responses when checking the cache.
data CacheResult a
  -- | The request hadn't been seen until now.
  = Uncached (ResultVar a)

  -- | The request has been seen before, but its result has not yet been
  -- fetched.
  | CachedNotFetched (ResultVar a)

  -- | The request has been seen before, and its result has already been
  -- fetched.
  | Cached (Either SomeException a)

-- | Checks the data cache for the result of a request.
cached :: (Request r a) => Env u -> r a -> IO (CacheResult a)
cached env req = do
  cache <- readIORef (cacheRef env)
  let
    do_fetch = do
      rvar <- newEmptyResult
      writeIORef (cacheRef env) $! DataCache.insert req rvar cache
      return (Uncached rvar)
  case DataCache.lookup req cache of
    Nothing -> do_fetch
    Just rvar -> do
      mb <- tryReadResult rvar
      case mb of
        Nothing -> return (CachedNotFetched rvar)
        -- Use the cached result, even if it was an error.
        Just r -> do
          ifTrace (flags env) 3 $ putStrLn $ case r of
            Left _ -> "Cached error: " ++ show req
            Right _ -> "Cached request: " ++ show req
          return (Cached r)

-- | Performs actual fetching of data for a 'Request' from a 'DataSource'.
dataFetch :: (DataSource u r, Request r a) => r a -> GenHaxl u a
dataFetch req = GenHaxl $ \env ref -> do
  -- First, check the cache
  res <- cached env req
  case res of
    -- Not seen before: add the request to the RequestStore, so it
    -- will be fetched in the next round.
    Uncached rvar -> do
      modifyIORef' ref $ \bs -> addRequest (BlockedFetch req rvar) bs
      return $ Blocked (Cont (continueFetch req rvar))

    -- Seen before but not fetched yet.  We're blocked, but we don't have
    -- to add the request to the RequestStore.
    CachedNotFetched rvar -> return
      $ Blocked (Cont (continueFetch req rvar))

    -- Cached: either a result, or an exception
    Cached (Left ex) -> return (Throw ex)
    Cached (Right a) -> return (Done a)

-- | A data request that is not cached.  This is not what you want for
-- normal read requests, because then multiple identical requests may
-- return different results, and this invalidates some of the
-- properties that we expect Haxl computations to respect: that data
-- fetches can be aribtrarily reordered, and identical requests can be
-- commoned up, for example.
--
-- 'uncachedRequest' is useful for performing writes, provided those
-- are done in a safe way - that is, not mixed with reads that might
-- conflict in the same Haxl computation.
--
uncachedRequest :: (DataSource u r, Request r a) => r a -> GenHaxl u a
uncachedRequest req = GenHaxl $ \_env ref -> do
  rvar <- newEmptyResult
  modifyIORef' ref $ \bs -> addRequest (BlockedFetch req rvar) bs
  return $ Blocked (Cont (continueFetch req rvar))

continueFetch
  :: (DataSource u r, Request r a, Show a)
  => r a -> ResultVar a -> GenHaxl u a
continueFetch req rvar = GenHaxl $ \_env _ref -> do
  m <- tryReadResult rvar
  case m of
    Nothing -> raise . DataSourceError $
      textShow req <> " did not set contents of result var"
    Just r -> done r

-- | Transparently provides caching. Useful for datasources that can
-- return immediately, but also caches values.  Exceptions thrown by
-- the IO operation (except for asynchronous exceptions) are
-- propagated into the Haxl monad and can be caught by 'catch' and
-- 'try'.
cacheResult :: (Request r a)  => r a -> IO a -> GenHaxl u a
cacheResult req val = GenHaxl $ \env _ref -> do
  cachedResult <- cached env req
  case cachedResult of
    Uncached rvar -> do
      result <- Exception.try val
      putResult rvar result
      case result of
        Left e -> do rethrowAsyncExceptions e; done result
        _other -> done result
    Cached result -> done result
    CachedNotFetched _ -> corruptCache
  where
    corruptCache = raise . DataSourceError $ Text.concat
      [ textShow req
      , " has a corrupted cache value: these requests are meant to"
      , " return immediately without an intermediate value. Either"
      , " the cache was updated incorrectly, or you're calling"
      , " cacheResult on a query that involves a blocking fetch."
      ]


-- We must be careful about turning IO monad exceptions into Haxl
-- exceptions.  An IO monad exception will normally propagate right
-- out of runHaxl and terminate the whole computation, whereas a Haxl
-- exception can get dropped on the floor, if it is on the right of
-- <*> and the left side also throws, for example.  So turning an IO
-- monad exception into a Haxl exception is a dangerous thing to do.
-- In particular, we never want to do it for an asynchronous exception
-- (AllocationLimitExceeded, ThreadKilled, etc.), because these are
-- supposed to unconditionally terminate the computation.
--
-- There are three places where we take an arbitrary IO monad exception and
-- turn it into a Haxl exception:
--
--  * wrapFetchInCatch.  Here we want to propagate a failure of the
--    data source to the callers of the data source, but if the
--    failure came from elsewhere (an asynchronous exception), then we
--    should just propagate it
--
--  * cacheResult (cache the results of IO operations): again,
--    failures of the IO operation should be visible to the caller as
--    a Haxl exception, but we exclude asynchronous exceptions from
--    this.

--  * unsafeToHaxlException: assume the caller knows what they're
--    doing, and just wrap all exceptions.
--
rethrowAsyncExceptions :: SomeException -> IO ()
rethrowAsyncExceptions e
#if __GLASGOW_HASKELL__ >= 708
  | Just SomeAsyncException{} <- fromException e = Exception.throw e
#endif
#if __GLASGOW_HASKELL__ >= 710
  | Just AllocationLimitExceeded{} <- fromException e = Exception.throw e
    -- AllocationLimitExceeded is not a child of SomeAsyncException,
    -- but it should be.
#endif
  | otherwise = return ()

-- | Inserts a request/result pair into the cache. Throws an exception
-- if the request has already been issued, either via 'dataFetch' or
-- 'cacheRequest'.
--
-- This can be used to pre-populate the cache when running tests, to
-- avoid going to the actual data source and ensure that results are
-- deterministic.
--
cacheRequest
  :: (Request req a) => req a -> Either SomeException a -> GenHaxl u ()
cacheRequest request result = GenHaxl $ \env _ref -> do
  res <- cached env request
  case res of
    Uncached rvar -> do
      -- request was not in the cache: insert the result and continue
      putResult rvar result
      return $ Done ()

    -- It is an error if the request is already in the cache.  We can't test
    -- whether the cached result is the same without adding an Eq constraint,
    -- and we don't necessarily have Eq for all results.
    _other -> raise $
      DataSourceError "cacheRequest: request is already in the cache"

instance IsString a => IsString (GenHaxl u a) where
  fromString s = return (fromString s)

-- | Issues a batch of fetches in a 'RequestStore'. After
-- 'performFetches', all the requests in the 'RequestStore' are
-- complete, and all of the 'ResultVar's are full.
performFetches :: forall u. Int -> Env u -> RequestStore u -> IO Int
performFetches n env reqs = do
  let f = flags env
      sref = statsRef env
      jobs = contents reqs
      !n' = n + length jobs

  t0 <- getCurrentTime

  let
    roundstats =
      [ (dataSourceName (getReq reqs), length reqs)
      | BlockedFetches reqs <- jobs ]
      where
      getReq :: [BlockedFetch r] -> r a
      getReq = undefined

  ifTrace f 1 $
    printf "Batch data fetch (%s)\n" $
      intercalate (", "::String) $
        map (\(name,num) -> printf "%d %s" num (Text.unpack name)) roundstats

  ifTrace f 3 $
    forM_ jobs $ \(BlockedFetches reqs) ->
      forM_ reqs $ \(BlockedFetch r _) -> putStrLn (show1 r)

  let
    applyFetch (i, BlockedFetches (reqs :: [BlockedFetch r])) =
      case stateGet (states env) of
        Nothing ->
          return (SyncFetch (mapM_ (setError (const e)) reqs))
          where req :: r a; req = undefined
                e = DataSourceError $
                      "data source not initialized: " <> dataSourceName req
        Just state ->
          return $ wrapFetchInTrace i (length reqs)
                    (dataSourceName (undefined :: r a))
                 $ wrapFetchInCatch reqs
                 $ fetch state f (userEnv env) reqs

  fetches <- mapM applyFetch $ zip [n..] jobs

  times <-
    if report f >= 2
    then do
      (refs, timedfetches) <- mapAndUnzipM wrapFetchInTimer fetches
      scheduleFetches timedfetches
      mapM (fmap Just . readIORef) refs
    else do
      scheduleFetches fetches
      return $ repeat Nothing

  let dsroundstats = HashMap.fromList
         [ (name, DataSourceRoundStats { dataSourceFetches = fetches
                                       , dataSourceTime = time
                                       })
         | ((name, fetches), time) <- zip roundstats times]

  t1 <- getCurrentTime
  let roundtime = realToFrac (diffUTCTime t1 t0) :: Double

  ifReport f 1 $
    modifyIORef' sref $ \(Stats rounds) -> roundstats `deepseq`
      Stats (RoundStats (microsecs roundtime) dsroundstats: rounds)

  ifTrace f 1 $
    printf "Batch data fetch done (%.2fs)\n" (realToFrac roundtime :: Double)

  return n'

-- Catch exceptions arising from the data source and stuff them into
-- the appropriate requests.  We don't want any exceptions propagating
-- directly from the data sources, because we want the exception to be
-- thrown by dataFetch instead.
--
wrapFetchInCatch :: [BlockedFetch req] -> PerformFetch -> PerformFetch
wrapFetchInCatch reqs fetch =
  case fetch of
    SyncFetch io ->
      SyncFetch (io `Exception.catch` handler)
    AsyncFetch fio ->
      AsyncFetch (\io -> fio io `Exception.catch` handler)
  where
    handler :: SomeException -> IO ()
    handler e = do
      rethrowAsyncExceptions e
      mapM_ (forceError e) reqs

    -- Set the exception even if the request already had a result.
    -- Otherwise we could be discarding an exception.
    forceError e (BlockedFetch _ rvar) = do
      void $ tryTakeResult rvar
      putResult rvar (except e)

wrapFetchInTimer :: PerformFetch -> IO (IORef Microseconds, PerformFetch)
wrapFetchInTimer f = do
  r <- newIORef 0
  case f of
    SyncFetch io -> return (r, SyncFetch (time io >>= writeIORef r))
    AsyncFetch f -> do
       inner_r <- newIORef 0
       return (r, AsyncFetch $ \inner -> do
         total <- time (f (time inner >>= writeIORef inner_r))
         inner_t <- readIORef inner_r
         writeIORef r (total - inner_t))

wrapFetchInTrace :: Int -> Int -> Text.Text -> PerformFetch -> PerformFetch
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

time :: IO () -> IO Microseconds
time io = do
  t0 <- getCurrentTime
  io
  t1 <- getCurrentTime
  return . microsecs . realToFrac $ t1 `diffUTCTime` t0

microsecs :: Double -> Microseconds
microsecs t = round (t * 10^(6::Int))

-- | Start all the async fetches first, then perform the sync fetches before
-- getting the results of the async fetches.
scheduleFetches :: [PerformFetch] -> IO()
scheduleFetches fetches = async_fetches sync_fetches
 where
  async_fetches :: IO () -> IO ()
  async_fetches = compose [f | AsyncFetch f <- fetches]

  sync_fetches :: IO ()
  sync_fetches = sequence_ [io | SyncFetch io <- fetches]


-- -----------------------------------------------------------------------------
-- Memoization

-- | A variable in the cache representing the state of a memoized computation
newtype MemoVar u a = MemoVar (IORef (MemoStatus u a))

-- | The state of a memoized computation
data MemoStatus u a
  = MemoInProgress (RoundId u) (GenHaxl u a)
      -- ^ Under evaluation in the given round, here is the latest
      -- continuation.  The continuation might be a little out of
      -- date, but that's fine, the worst that can happen is we do a
      -- little extra work.
  | MemoDone (Either SomeException a)
      -- fully evaluated, here is the result.

type RoundId u = IORef (RequestStore u)
{-
Note [RoundId]

A token representing the round.  This needs to be unique per round,
and it needs to support Eq.  Fortunately the IORef RequestStore is
exactly what we need: IORef supports Eq, and we make a new one for
each round.  There's a danger that storing this in the DataCache could
cause a space leak, so we stub out the contents after each round (see
runHaxl).
-}

-- | 'cachedComputation' memoizes a Haxl computation.  The key is a
-- request.
--
-- /Note:/ These cached computations will /not/ be included in the output
-- of 'dumpCacheAsHaskell'.
--
cachedComputation
   :: forall req u a.
      (Eq (req a)
      , Hashable (req a)
      , Typeable (req a))
   => req a -> GenHaxl u a -> GenHaxl u a

cachedComputation req haxl = GenHaxl $ \env ref -> do
  cache <- readIORef (memoRef env)
  case DataCache.lookup req cache of
    Nothing -> do
      memovar <- newIORef (MemoInProgress ref haxl)
      writeIORef (memoRef env) $!
        DataCache.insertNotShowable req (MemoVar memovar) cache
      run memovar haxl env ref
    Just (MemoVar memovar) -> do
      status <- readIORef memovar
      case status of
        MemoDone r -> done r
        MemoInProgress round cont
          | round == ref -> return (Blocked (Cont (retryMemo req)))
          | otherwise    -> run memovar cont env ref
          -- was blocked in a previous round; run the saved continuation to
          -- make more progress.
 where
  -- If we got blocked on this memo in the current round, this is the
  -- continuation: just try to evaluate the memo again.  We know it is
  -- already in the cache (because we just checked), so the computation
  -- will never be used.
  retryMemo :: (Typeable (req a)) => req a -> GenHaxl u a
  retryMemo req =
   cachedComputation req (throw (CriticalError "retryMemo"))

  -- Run the memoized computation and store the result (complete or
  -- partial) back in the MemoVar afterwards.
 --
 -- We don't attempt to catch IO monad exceptions here.  That may seem
 -- dangerous, because if an IO exception is raised we'll leave the
 -- MemoInProgress in the MemoVar.  But we always want to just
 -- propagate an IO monad exception (it should kill the whole runHaxl,
 -- unless there's a unsafeToHaxlException), so we should never be
 -- looking at the MemoVar again anyway.  Furthermore, storing the
 -- exception in the MemoVar is wrong, because that will turn it into
 -- a Haxl exception (see rethrowAsyncExceptions).
  run memovar cont env ref = do
    e <- unHaxl cont env ref
    case e of
      Done a -> complete memovar (Right a)
      Throw e -> complete memovar (Left e)
      Blocked cont -> do
        writeIORef memovar (MemoInProgress ref (toHaxl cont))
        return (Blocked (Cont (retryMemo req)))

  -- We're finished: store the final result
  complete memovar r = do
    writeIORef memovar (MemoDone r)
    done r

-- | Lifts an 'Either' into either 'Throw' or 'Done'.
done :: Either SomeException a -> IO (Result u a)
done = return . either Throw Done


-- -----------------------------------------------------------------------------

-- | Dump the contents of the cache as Haskell code that, when
-- compiled and run, will recreate the same cache contents.  For
-- example, the generated code looks something like this:
--
-- > loadCache :: GenHaxl u ()
-- > loadCache = do
-- >   cacheRequest (ListWombats 3) (Right ([1,2,3]))
-- >   cacheRequest (CountAardvarks "abcabc") (Right (2))
--
dumpCacheAsHaskell :: GenHaxl u String
dumpCacheAsHaskell = do
  ref <- env cacheRef  -- NB. cacheRef, not memoRef.  We ignore memoized
                       -- results when dumping the cache.
  entries <- unsafeLiftIO $ readIORef ref >>= showCache
  let
    mk_cr (req, res) =
      text "cacheRequest" <+> parens (text req) <+> parens (result res)
    result (Left e) = text "except" <+> parens (text (show e))
    result (Right s) = text "Right" <+> parens (text s)

  return $ show $
    text "loadCache :: GenHaxl u ()" $$
    text "loadCache = do" $$
      nest 2 (vcat (map mk_cr (concatMap snd entries))) $$
    text "" -- final newline
