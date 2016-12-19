-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file. An additional grant of patent rights can
-- be found in the PATENTS file.

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
#if __GLASGOW_HASKELL >= 800
{-# OPTIONS_GHC -Wno-name-shadowing #-}
#else
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
#endif

-- | The implementation of the 'Haxl' monad.  Most users should
-- import "Haxl.Core" instead of importing this module directly.
module Haxl.Core.Monad (
    -- * The monad
    GenHaxl (..), runHaxl,
    env, withEnv, withLabel, withFingerprintLabel,

    -- * Env
    Env(..), Caches, caches, initEnvWithData, initEnv, emptyEnv,

    -- * Exceptions
    throw, catch, catchIf, try, tryToHaxlException,

    -- * Data fetching and caching
    ShowReq, dataFetch, dataFetchWithShow, uncachedRequest, cacheRequest,
    cacheResult, cacheResultWithShow, cachedComputation,
    dumpCacheAsHaskell, dumpCacheAsHaskellFn,

    -- * Memoization Machinery
    newMemo, newMemoWith, prepareMemo, runMemo,

    newMemo1, newMemoWith1, prepareMemo1, runMemo1,
    newMemo2, newMemoWith2, prepareMemo2, runMemo2,

    -- * Unsafe operations
    unsafeLiftIO, unsafeToHaxlException,
  ) where

import Haxl.Core.Types
import Haxl.Core.ShowP
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
import GHC.Conc (getAllocationCounter, setAllocationCounter)
#endif
import Control.Monad
import qualified Control.Exception as Exception
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative hiding (Const)
#endif
import Control.DeepSeq
import GHC.Exts (IsString(..), Addr#)
#if __GLASGOW_HASKELL__ < 706
import Prelude hiding (catch)
#endif
import Data.Functor.Constant
import Data.Hashable
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.IORef
import Data.List
import qualified Data.Map as Map
import Data.Monoid
import Data.Time
import Data.Typeable
import Text.Printf
import Text.PrettyPrint hiding ((<>))
import Control.Arrow (left)

#ifdef EVENTLOG
import Control.Exception (bracket_)
import Debug.Trace (traceEventIO)
#endif

#ifdef PROFILING
import GHC.Stack
#endif

#if __GLASGOW_HASKELL__ < 710
import Data.Int (Int64)

getAllocationCounter :: IO Int64
getAllocationCounter = return 0

setAllocationCounter :: Int64 -> IO ()
setAllocationCounter _ = return ()
#endif

-- -----------------------------------------------------------------------------
-- The environment

-- | The data we carry around in the Haxl monad.
data Env u = Env
  { cacheRef     :: {-# UNPACK #-} !(IORef (DataCache ResultVar))
                     -- cached data fetches
  , memoRef      :: {-# UNPACK #-} !(IORef (DataCache (MemoVar u)))
                     -- memoized computations
  , flags        :: !Flags
                     -- conservatively not unpacking, because this is passed
                     -- to 'fetch' and would need to be rebuilt.
  , userEnv      :: u
  , statsRef     :: {-# UNPACK #-} !(IORef Stats)
  , profLabel    :: ProfileLabel
  , profRef      :: {-# UNPACK #-} !(IORef Profile)
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
  pref <- newIORef emptyProfile
  return Env
    { cacheRef = cref
    , memoRef = mref
    , flags = defaultFlags
    , userEnv = e
    , states = states
    , statsRef = sref
    , profLabel = "MAIN"
    , profRef = pref
    }

-- | Initializes an environment with 'StateStore' and an input map.
initEnv :: StateStore -> u -> IO (Env u)
initEnv states e = do
  cref <- newIORef emptyDataCache
  mref <- newIORef emptyDataCache
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
  fail msg = GenHaxl $ \_env _ref ->
    return $ Throw $ toException $ MonadFail $ Text.pack msg

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
        e <- (unHaxl $ toHaxl c) env ref
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
            when (caching (flags env) == 0) $
              writeIORef (cacheRef env) DataCache.empty
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
      when (caching (flags env) == 0) $
        writeIORef (cacheRef env) emptyDataCache
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

-- | Label a computation so profiling data is attributed to the label.
withLabel :: ProfileLabel -> GenHaxl u a -> GenHaxl u a
withLabel l (GenHaxl m) = GenHaxl $ \env ref ->
  if report (flags env) < 4
     then m env ref
     else collectProfileData l m env ref

-- | Label a computation so profiling data is attributed to the label.
-- Intended only for internal use by 'memoFingerprint'.
withFingerprintLabel :: Addr# -> Addr# -> GenHaxl u a -> GenHaxl u a
withFingerprintLabel mnPtr nPtr (GenHaxl m) = GenHaxl $ \env ref ->
  if report (flags env) < 4
     then m env ref
     else collectProfileData
            (Text.unpackCString# mnPtr <> "." <> Text.unpackCString# nPtr)
            m env ref

-- | Collect profiling data and attribute it to given label.
collectProfileData
  :: ProfileLabel
  -> (Env u -> IORef (RequestStore u) -> IO (Result u a))
  -> Env u -> IORef (RequestStore u)
  -> IO (Result u a)
collectProfileData l m env ref = do
   a0 <- getAllocationCounter
   r <- m env{profLabel=l} ref -- what if it throws?
   a1 <- getAllocationCounter
   modifyProfileData env l (a0 - a1)
   -- So we do not count the allocation overhead of modifyProfileData
   setAllocationCounter a1
   case r of
     Done a -> return (Done a)
     Throw e -> return (Throw e)
     Blocked k -> return (Blocked (Cont (withLabel l (toHaxl k))))
{-# INLINE collectProfileData #-}

modifyProfileData :: Env u -> ProfileLabel -> AllocCount -> IO ()
modifyProfileData env label allocs =
  modifyIORef' (profRef env) $ \ p ->
    p { profile =
          HashMap.insertWith updEntry label newEntry .
          HashMap.insertWith updCaller caller newCaller $
          profile p }
  where caller = profLabel env
        newEntry =
          emptyProfileData
            { profileAllocs = allocs
            , profileDeps = HashSet.singleton caller }
        updEntry _ old =
          old { profileAllocs = profileAllocs old + allocs
              , profileDeps = HashSet.insert caller (profileDeps old) }
        -- subtract allocs from caller, so they are not double counted
        -- we don't know the caller's caller, but it will get set on
        -- the way back out, so an empty hashset is fine for now
        newCaller =
          emptyProfileData { profileAllocs = -allocs }
        updCaller _ old =
          old { profileAllocs = profileAllocs old - allocs }

incrementMemoHitCounterFor :: ProfileLabel -> Profile -> Profile
incrementMemoHitCounterFor lbl p =
  p { profile = HashMap.adjust incrementMemoHitCounter lbl (profile p) }

incrementMemoHitCounter :: ProfileData -> ProfileData
incrementMemoHitCounter pd = pd { profileMemoHits = succ (profileMemoHits pd) }

-- -----------------------------------------------------------------------------
-- Exceptions

-- | Throw an exception in the Haxl monad
throw :: (Exception e) => e -> GenHaxl u a
throw e = GenHaxl $ \_env _ref -> raise e

raise :: (Exception e) => e -> IO (Result u a)
raise e
#ifdef PROFILING
  | Just (HaxlException Nothing h) <- fromException somex = do
    stk <- currentCallStack
    return (Throw (toException (HaxlException (Just stk) h)))
  | otherwise
#endif
    = return (Throw somex)
  where
    somex = toException e

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
cached :: Request r a => Env u -> r a -> IO (CacheResult a)
cached = cachedWithInsert show DataCache.insert

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

-- | Checks the data cache for the result of a request, inserting new results
-- with the given function.
cachedWithInsert
  :: Typeable (r a)
  => (r a -> String)    -- See Note [showFn]
  -> (r a -> ResultVar a -> DataCache ResultVar -> DataCache ResultVar) -> Env u
  -> r a -> IO (CacheResult a)
cachedWithInsert showFn insertFn env req = do
  let
    doFetch insertFn request cache = do
      rvar <- newEmptyResult
      writeIORef (cacheRef env) $! insertFn request rvar cache
      return (Uncached rvar)
  cache <- readIORef (cacheRef env)
  case DataCache.lookup req cache of
    Nothing -> doFetch insertFn req cache
    Just rvar -> do
      mb <- tryReadResult rvar
      case mb of
        Nothing -> return (CachedNotFetched rvar)
        -- Use the cached result, even if it was an error.
        Just r -> do
          ifTrace (flags env) 3 $ putStrLn $ case r of
            Left _ -> "Cached error: " ++ showFn req
            Right _ -> "Cached request: " ++ showFn req
          return (Cached r)

-- | Record the call stack for a data fetch in the Stats.  Only useful
-- when profiling.
logFetch :: Env u -> (r a -> String) -> r a -> IO ()
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
dataFetch :: (DataSource u r, Request r a) => r a -> GenHaxl u a
dataFetch = dataFetchWithInsert show DataCache.insert

-- | Performs actual fetching of data for a 'Request' from a 'DataSource', using
-- the given show functions for requests and their results.
dataFetchWithShow
  :: (DataSource u r, Eq (r a), Hashable (r a), Typeable (r a))
  => ShowReq r a
  -> r a -> GenHaxl u a
dataFetchWithShow (showReq, showRes) = dataFetchWithInsert showReq
  (DataCache.insertWithShow showReq showRes)

-- | Performs actual fetching of data for a 'Request' from a 'DataSource', using
-- the given function to insert requests in the cache.
dataFetchWithInsert
  :: (DataSource u r, Eq (r a), Hashable (r a), Typeable (r a))
  => (r a -> String)    -- See Note [showFn]
  -> (r a -> ResultVar a -> DataCache ResultVar -> DataCache ResultVar)
  -> r a
  -> GenHaxl u a
dataFetchWithInsert showFn insertFn req = GenHaxl $ \env ref -> do
  -- First, check the cache
  res <- cachedWithInsert showFn insertFn env req
  ifProfiling (flags env) $ addProfileFetch env req
  case res of
    -- Not seen before: add the request to the RequestStore, so it
    -- will be fetched in the next round.
    Uncached rvar -> do
      logFetch env showFn req
      modifyIORef' ref $ \bs -> addRequest (BlockedFetch req rvar) bs
      return $ Blocked (Cont (continueFetch showFn req rvar))

    -- Seen before but not fetched yet.  We're blocked, but we don't have
    -- to add the request to the RequestStore.
    CachedNotFetched rvar ->
      return (Blocked (Cont (continueFetch showFn req rvar)))

    -- Cached: either a result, or an exception
    Cached (Left ex) -> return (Throw ex)
    Cached (Right a) -> return (Done a)

{-# NOINLINE addProfileFetch #-}
addProfileFetch
  :: (DataSourceName r, Eq (r a), Hashable (r a), Typeable (r a))
  => Env u -> r a -> IO ()
addProfileFetch env req = do
  c <- getAllocationCounter
  modifyIORef' (profRef env) $ \ p ->
    let
      dsName :: Text.Text
      dsName = dataSourceName req

      upd :: Round -> ProfileData -> ProfileData
      upd round d =
        d { profileFetches = Map.alter (Just . f) round (profileFetches d) }

      f Nothing   = HashMap.singleton dsName 1
      f (Just hm) = HashMap.insertWith (+) dsName 1 hm
    in case DataCache.lookup req (profileCache p) of
        Nothing ->
          let r = profileRound p
          in p { profile = HashMap.adjust (upd r) (profLabel env) (profile p)
               , profileCache =
                  DataCache.insertNotShowable req (Constant r) (profileCache p)
               }
        Just (Constant r) ->
          p { profile = HashMap.adjust (upd r) (profLabel env) (profile p) }
  -- So we do not count the allocation overhead of addProfileFetch
  setAllocationCounter c

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
uncachedRequest :: (DataSource u r, Show (r a)) => r a -> GenHaxl u a
uncachedRequest req = GenHaxl $ \_env ref -> do
  rvar <- newEmptyResult
  modifyIORef' ref $ \bs -> addRequest (BlockedFetch req rvar) bs
  return $ Blocked (Cont (continueFetch show req rvar))

continueFetch
  :: (r a -> String)    -- See Note [showFn]
  -> r a -> ResultVar a -> GenHaxl u a
continueFetch showFn req rvar = GenHaxl $ \_env _ref -> do
  m <- tryReadResult rvar
  case m of
    Nothing -> raise . DataSourceError $
      Text.pack (showFn req) <> " did not set contents of result var"
    Just r -> done r

-- | Transparently provides caching. Useful for datasources that can
-- return immediately, but also caches values.  Exceptions thrown by
-- the IO operation (except for asynchronous exceptions) are
-- propagated into the Haxl monad and can be caught by 'catch' and
-- 'try'.
cacheResult :: Request r a => r a -> IO a -> GenHaxl u a
cacheResult = cacheResultWithInsert show DataCache.insert

-- | Transparently provides caching in the same way as 'cacheResult', but uses
-- the given functions to show requests and their results.
cacheResultWithShow
  :: (Eq (r a), Hashable (r a), Typeable (r a))
  => ShowReq r a -> r a -> IO a -> GenHaxl u a
cacheResultWithShow (showReq, showRes) = cacheResultWithInsert showReq
  (DataCache.insertWithShow showReq showRes)

-- Transparently provides caching, using the given function to insert requests
-- into the cache.
cacheResultWithInsert
  :: Typeable (r a)
  => (r a -> String)    -- See Note [showFn]
  -> (r a -> ResultVar a -> DataCache ResultVar -> DataCache ResultVar) -> r a
  -> IO a -> GenHaxl u a
cacheResultWithInsert showFn insertFn req val = GenHaxl $ \env _ref -> do
  cachedResult <- cachedWithInsert showFn insertFn env req
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
      [ Text.pack (showFn req)
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
  :: Request req a => req a -> Either SomeException a -> GenHaxl u ()
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
  a0 <- getAllocationCounter

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
      forM_ reqs $ \(BlockedFetch r _) -> putStrLn (showp r)

  let
    applyFetch (i, BlockedFetches (reqs :: [BlockedFetch r])) =
      case stateGet (states env) of
        Nothing ->
          return (SyncFetch (mapM_ (setError e) reqs))
          where e req = DataSourceError $
                  "data source not initialized: " <>
                  dataSourceName req <>
                  ": " <>
                  Text.pack (showp req)
        Just state ->
          return $ wrapFetchInTrace i (length reqs)
                    (dataSourceName (undefined :: r a))
                 $ wrapFetchInCatch reqs
                 $ fetch state f (userEnv env) reqs

  fetches <- mapM applyFetch $ zip [n..] jobs

  deepStats <-
    if report f >= 2
    then do
      (refs, timedfetches) <- mapAndUnzipM wrapFetchInStats fetches
      scheduleFetches timedfetches
      mapM (fmap Just . readIORef) refs
    else do
      scheduleFetches fetches
      return $ repeat Nothing

  failures <-
    if report f >= 3
    then
      forM jobs $ \(BlockedFetches reqs) ->
        fmap (Just . length) . flip filterM reqs $ \(BlockedFetch _ rvar) -> do
          mb <- tryReadResult rvar
          return $ case mb of
            Just (Right _) -> False
            _ -> True
    else return $ repeat Nothing

  let dsroundstats = HashMap.fromList
         [ (name, DataSourceRoundStats { dataSourceFetches = dsfetch
                                       , dataSourceTime = fst <$> dsStats
                                       , dataSourceAllocation = snd <$> dsStats
                                       , dataSourceFailures = dsfailure
                                       })
         | ((name, dsfetch), dsStats, dsfailure) <-
             zip3 roundstats deepStats failures]

  a1 <- getAllocationCounter
  t1 <- getCurrentTime
  let
    roundtime = realToFrac (diffUTCTime t1 t0) :: Double
    allocation = fromIntegral $ a0 - a1

  ifReport f 1 $
    modifyIORef' sref $ \(Stats rounds) -> roundstats `deepseq`
      Stats (RoundStats (microsecs roundtime) allocation dsroundstats: rounds)

  ifTrace f 1 $
    printf "Batch data fetch done (%.2fs)\n" (realToFrac roundtime :: Double)

  ifProfiling f $
    modifyIORef' (profRef env) $ \ p -> p { profileRound = 1 + profileRound p }

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

wrapFetchInStats :: PerformFetch -> IO (IORef (Microseconds, Int), PerformFetch)
wrapFetchInStats f = do
  r <- newIORef (0, 0)
  case f of
    SyncFetch io -> return (r, SyncFetch (statsForIO io >>= writeIORef r))
    AsyncFetch f -> do
       inner_r <- newIORef (0, 0)
       return (r, AsyncFetch $ \inner -> do
         (totalTime, totalAlloc) <-
           statsForIO (f (statsForIO inner >>= writeIORef inner_r))
         (innerTime, innerAlloc) <- readIORef inner_r
         writeIORef r (totalTime - innerTime, totalAlloc - innerAlloc))
  where
    statsForIO io = do
      prevAlloc <- getAllocationCounter
      t <- time io
      postAlloc <- getAllocationCounter
      return (t, fromIntegral $ prevAlloc - postAlloc)

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

-- | Variables representing memoized computations.
newtype MemoVar u a = MemoVar (IORef (MemoStatus u a))
newtype MemoVar1 u a b = MemoVar1 (IORef (MemoStatus1 u a b))
newtype MemoVar2 u a b c = MemoVar2 (IORef (MemoStatus2 u a b c))

-- | The state of a memoized computation
data MemoStatus u a
  -- | Memoized computation under evaluation. The memo was last evaluated during
  -- the given round, or never, if the given round is Nothing. The continuation
  -- might be slightly out of date, but that's fine; the worst that can happen
  -- is we do a little extra work.
  = MemoInProgress (RoundId u) (GenHaxl u a)

  -- | A fully evaluated memo; here is the result.
  | MemoDone (Either SomeException a)

  -- | A new memo, with a stored computation. Not empty, but has not been run
  -- yet.
  | MemoNew (GenHaxl u a)

  -- | An empty memo, should not be run before preparation.
  | MemoEmpty

-- | The state of a memoized 1-argument function.
data MemoStatus1 u a b
  -- | An unprepared memo.
  = MemoEmpty1
  -- | A memo-table containing @MemoStatus@es for at least one in-progress memo.
  | MemoTbl1 ( a -> GenHaxl u b
             , HashMap.HashMap a
               (MemoVar u b))

data MemoStatus2 u a b c
  -- | An unprepared memo.
  = MemoEmpty2
  -- | A memo-table containing @MemoStatus@es for at least one in-progress memo.
  | MemoTbl2 ( a -> b -> GenHaxl u c
             , HashMap.HashMap a
               (HashMap.HashMap b
                 (MemoVar u c)))

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
      ( Eq (req a)
      , Hashable (req a)
      , Typeable (req a))
   => req a -> GenHaxl u a -> GenHaxl u a
cachedComputation req haxl = do
  env <- env id
  cache <- unsafeLiftIO $ readIORef (memoRef env)
  unsafeLiftIO $ ifProfiling (flags env) $
    modifyIORef' (profRef env) (incrementMemoHitCounterFor (profLabel env))
  memoVar <- case DataCache.lookup req cache of
               Nothing -> do
                 memoVar <- newMemoWith haxl
                 unsafeLiftIO $ writeIORef (memoRef env) $!
                   DataCache.insertNotShowable req memoVar cache
                 return memoVar
               Just memoVar -> return memoVar
  runMemo memoVar

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
dumpCacheAsHaskell = dumpCacheAsHaskellFn "loadCache" "GenHaxl u ()"

-- | Dump the contents of the cache as Haskell code that, when
-- compiled and run, will recreate the same cache contents.
--
-- Takes the name and type for the resulting function as arguments.
dumpCacheAsHaskellFn :: String -> String -> GenHaxl u String
dumpCacheAsHaskellFn fnName fnType = do
  ref <- env cacheRef  -- NB. cacheRef, not memoRef.  We ignore memoized
                       -- results when dumping the cache.
  entries <- unsafeLiftIO $ readIORef ref >>= showCache
  let
    mk_cr (req, res) =
      text "cacheRequest" <+> parens (text req) <+> parens (result res)
    result (Left e) = text "except" <+> parens (text (show e))
    result (Right s) = text "Right" <+> parens (text s)

  return $ show $
    text (fnName ++ " :: " ++ fnType) $$
    text (fnName ++ " = do") $$
      nest 2 (vcat (map mk_cr (concatMap snd entries))) $$
    text "" -- final newline

-- | Create a new @MemoVar@ for storing a memoized computation. The created
-- @MemoVar@ is initially empty, not tied to any specific computation. Running
-- this memo (with @runMemo@) without preparing it first (with @prepareMemo@)
-- will result in an exception.
newMemo :: GenHaxl u (MemoVar u a)
newMemo = unsafeLiftIO $ MemoVar <$> newIORef MemoEmpty

-- | Store a computation within a supplied @MemoVar@. Any memo stored within the
-- @MemoVar@ already (regardless of completion) will be discarded, in favor of
-- the supplied computation. A @MemoVar@ must be prepared before it is run.
prepareMemo :: MemoVar u a -> GenHaxl u a -> GenHaxl u ()
prepareMemo (MemoVar memoRef) memoCmp
  = unsafeLiftIO $ writeIORef memoRef (MemoNew memoCmp)

-- | Convenience function, combines @newMemo@ and @prepareMemo@.
newMemoWith :: GenHaxl u a -> GenHaxl u (MemoVar u a)
newMemoWith memoCmp = do
  memoVar <- newMemo
  prepareMemo memoVar memoCmp
  return memoVar

-- | Continue the memoized computation within a given @MemoVar@.
-- Notes:
--
--   1. If the memo contains a complete result, return that result.
--   2. If the memo contains an in-progress computation, continue it as far as
--      possible for this round.
--   3. If the memo is empty (it was not prepared), throw an error.
--
-- For example, to memoize the computation @one@ given by:
--
-- > one :: Haxl Int
-- > one = return 1
--
-- use:
--
-- > do
-- >   oneMemo <- newMemoWith one
-- >   let memoizedOne = runMemo aMemo one
-- >   oneResult <- memoizedOne
--
-- To memoize mutually dependent computations such as in:
--
-- > h :: Haxl Int
-- > h = do
-- >   a <- f
-- >   b <- g
-- >   return (a + b)
-- >  where
-- >   f = return 42
-- >   g = succ <$> f
--
-- without needing to reorder them, use:
--
-- > h :: Haxl Int
-- > h = do
-- >   fMemoRef <- newMemo
-- >   gMemoRef <- newMemo
-- >
-- >   let f = runMemo fMemoRef
-- >       g = runMemo gMemoRef
-- >
-- >   prepareMemo fMemoRef $ return 42
-- >   prepareMemo gMemoRef $ succ <$> f
-- >
-- >   a <- f
-- >   b <- g
-- >   return (a + b)
--
runMemo :: MemoVar u a -> GenHaxl u a
runMemo memoVar@(MemoVar memoRef) = GenHaxl $ \env rID ->
  readIORef memoRef >>= \case
    -- Memo was not prepared first; throw an exception.
    MemoEmpty -> raise $ CriticalError "Attempting to run empty memo."
    -- The memo is complete.
    MemoDone result -> done result
    -- Memo has just been prepared, run it.
    MemoNew cont -> runContToMemo cont env rID
    -- The memo is in progress, there *may* be progress to be made.
    MemoInProgress rID' cont
      -- The last update was performed *this* round and is still in progress;
      -- nothing further can be done this round. Wait until the next round.
      | rID' == rID -> return (Blocked $ Cont retryMemo)
      -- This is the first time this memo is being run during this round, or
      -- at all. Enough progress may have been made to continue running the
      -- memo.
      | otherwise -> runContToMemo cont env rID
 where
  -- Continuation to retry an existing memo. It is not possible to *retry* an
  -- empty memo; that will throw an exception during the next round.
  retryMemo = runMemo memoVar

  -- Run a continuation, and store the result in the memo reference. Any
  -- exceptions thrown during the running of the memo are thrown directly; they
  -- are also stored in the memoVar just in case, but we shouldn't be looking at
  -- the memoVar again anyway.
  --
  -- If the memo is incomplete by the end of this round, update its progress
  -- indicator and block.
  runContToMemo cont env rID = do
    result <- unHaxl cont env rID
    case result of
      Done a -> finalize (Right a)
      Throw e -> finalize (Left e)
      Blocked c -> do
        writeIORef memoRef (MemoInProgress rID (toHaxl c))
        return (Blocked $ Cont retryMemo)

  finalize r = writeIORef memoRef (MemoDone r) >> done r

newMemo1 :: GenHaxl u (MemoVar1 u a b)
newMemo1 = unsafeLiftIO $ MemoVar1 <$> newIORef MemoEmpty1

newMemoWith1 :: (a -> GenHaxl u b) -> GenHaxl u (MemoVar1 u a b)
newMemoWith1 f = newMemo1 >>= \r -> prepareMemo1 r f >> return r

prepareMemo1 :: MemoVar1 u a b -> (a -> GenHaxl u b) -> GenHaxl u ()
prepareMemo1 (MemoVar1 r) f
  = unsafeLiftIO $ writeIORef r (MemoTbl1 (f, HashMap.empty))

runMemo1 :: (Eq a, Hashable a) => MemoVar1 u a b -> a -> GenHaxl u b
runMemo1 (MemoVar1 r) k = unsafeLiftIO (readIORef r) >>= \case
  MemoEmpty1 -> throw $ CriticalError "Attempting to run empty memo."
  MemoTbl1 (f, h) -> case HashMap.lookup k h of
    Nothing -> do
      x <- newMemoWith (f k)
      unsafeLiftIO $ writeIORef r (MemoTbl1 (f, HashMap.insert k x h))
      runMemo x
    Just v -> runMemo v

newMemo2 :: GenHaxl u (MemoVar2 u a b c)
newMemo2 = unsafeLiftIO $ MemoVar2 <$> newIORef MemoEmpty2

newMemoWith2 :: (a -> b -> GenHaxl u c) -> GenHaxl u (MemoVar2 u a b c)
newMemoWith2 f = newMemo2 >>= \r -> prepareMemo2 r f >> return r

prepareMemo2 :: MemoVar2 u a b c -> (a -> b -> GenHaxl u c) -> GenHaxl u ()
prepareMemo2 (MemoVar2 r) f
  = unsafeLiftIO $ writeIORef r (MemoTbl2 (f, HashMap.empty))

runMemo2 :: (Eq a, Hashable a, Eq b, Hashable b)
         => MemoVar2 u a b c
         -> a -> b -> GenHaxl u c
runMemo2 (MemoVar2 r) k1 k2 = unsafeLiftIO (readIORef r) >>= \case
  MemoEmpty2 -> throw $ CriticalError "Attempting to run empty memo."
  MemoTbl2 (f, h1) -> case HashMap.lookup k1 h1 of
    Nothing -> do
      v <- newMemoWith (f k1 k2)
      unsafeLiftIO $ writeIORef r
        (MemoTbl2 (f, HashMap.insert k1 (HashMap.singleton k2 v) h1))
      runMemo v
    Just h2 -> case HashMap.lookup k2 h2 of
      Nothing -> do
        v <- newMemoWith (f k1 k2)
        unsafeLiftIO $ writeIORef r
          (MemoTbl2 (f, HashMap.insert k1 (HashMap.insert k2 v h2) h1))
        runMemo v
      Just v -> runMemo v
