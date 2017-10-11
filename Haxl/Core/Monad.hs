-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file. An additional grant of patent rights can
-- be found in the PATENTS file.

{- TODO

- do EVENTLOG stuff, track the data fetch numbers for performFetch

- timing: we should be using clock_gettime(CLOCK_MONOTONIC) instead of
  getCurrentTime, which will be affected by NTP and leap seconds.

- write different scheduling policies

-}

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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
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

    -- * IVar
    IVar(..), IVarContents(..), ResultVal(..),

    -- * Env
    Env(..), Caches, caches, initEnvWithData, initEnv, emptyEnv,

    -- * Exceptions
    throw, catch, catchIf, try, tryToHaxlException,

    -- * Data fetching and caching
    ShowReq, dataFetch, dataFetchWithShow, uncachedRequest, cacheRequest,
    cacheResult, cacheResultWithShow, cachedComputation, preCacheComputation,
    dumpCacheAsHaskell, dumpCacheAsHaskellFn,

    -- * Memoization Machinery
    newMemo, newMemoWith, prepareMemo, runMemo,

    newMemo1, newMemoWith1, prepareMemo1, runMemo1,
    newMemo2, newMemoWith2, prepareMemo2, runMemo2,

    -- * Unsafe operations
    unsafeLiftIO, unsafeToHaxlException,

    -- * Parallel operaitons
    pAnd, pOr
  ) where

import Haxl.Core.Types
import Haxl.Core.ShowP
import Haxl.Core.StateStore
import Haxl.Core.Exception
import Haxl.Core.RequestStore as RequestStore
import Haxl.Core.Util
import Haxl.Core.DataCache as DataCache

import Control.Arrow (left)
import Control.Concurrent.STM
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Control.Monad.Catch as Catch
import Control.Exception (Exception(..), SomeException, throwIO)
#if __GLASGOW_HASKELL__ >= 710
import GHC.Conc (getAllocationCounter, setAllocationCounter)
#endif
import Control.Monad
import qualified Control.Exception as Exception
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative hiding (Const)
#endif
import GHC.Exts (IsString(..), Addr#)
#if __GLASGOW_HASKELL__ < 706
import Prelude hiding (catch)
#endif
import Data.Either
import Data.Hashable
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.IORef
import Data.List
import Data.Monoid
import Data.Typeable
import Text.PrettyPrint hiding ((<>))
import Text.Printf
#ifdef EVENTLOG
import Control.Exception (bracket_)
import Debug.Trace (traceEventIO)
#endif

#ifdef PROFILING
import GHC.Stack
#endif

import Unsafe.Coerce

#if __GLASGOW_HASKELL__ < 710
import Data.Int (Int64)

getAllocationCounter :: IO Int64
getAllocationCounter = return 0

setAllocationCounter :: Int64 -> IO ()
setAllocationCounter _ = return ()
#endif


trace_ :: String -> a -> a
trace_ _ = id
--trace_ = Debug.Trace.trace

-- -----------------------------------------------------------------------------
-- The environment

-- | The data we carry around in the Haxl monad.
data Env u = Env
  { cacheRef     :: {-# UNPACK #-} !(IORef (DataCache (IVar u)))
      -- ^ cached data fetches

  , memoRef      :: {-# UNPACK #-} !(IORef (DataCache (IVar u)))
      -- ^ memoized computations

  , flags        :: !Flags
      -- conservatively not unpacking, because this is passed
      -- to 'fetch' and would need to be rebuilt.

  , userEnv      :: u
      -- ^ user-supplied data, retrievable with 'env'

  , statsRef     :: {-# UNPACK #-} !(IORef Stats)
      -- ^ statistics, collected according to the 'report' level in 'flags'.

  , profLabel    :: ProfileLabel
      -- ^ current profiling label, see 'withLabel'

  , profRef      :: {-# UNPACK #-} !(IORef Profile)
      -- ^ profiling data, collected according to the 'report' level in 'flags'.

  , states       :: StateStore
      -- ^ Data sources and other components can store their state in
      -- here. Items in this store must be instances of 'StateKey'.

  , reqStoreRef :: {-# UNPACK #-} !(IORef (RequestStore u))
       -- ^ The set of requests that we have not submitted to data sources yet.
       -- Owned by the scheduler.

  , runQueueRef :: {-# UNPACK #-} !(IORef (JobList u))
       -- ^ runnable computations. Things get added to here when we wake up
       -- a computation that was waiting for something.  When the list is
       -- empty, either we're finished, or we're waiting for some data fetch
       -- to return.

  , completions :: {-# UNPACK #-} !(TVar [CompleteReq u])
       -- ^ Requests that have completed.  Modified by data sources
       -- (via putResult) and the scheduler.  Waiting for this list to
       -- become non-empty is how the scheduler blocks waiting for
       -- data fetches to return.

  , pendingWaits :: [IO ()]
       -- ^ this is a list of IO actions returned by 'FutureFetch'
       -- data sources.  These do a blocking wait for the results of
       -- some data fetch.

  , speculative :: {-# UNPACK #-} !Int
  }

type Caches u = (IORef (DataCache (IVar u)), IORef (DataCache (IVar u)))

caches :: Env u -> Caches u
caches env = (cacheRef env, memoRef env)

-- | Initialize an environment with a 'StateStore', an input map, a
-- preexisting 'DataCache', and a seed for the random number generator.
initEnvWithData :: StateStore -> u -> Caches u -> IO (Env u)
initEnvWithData states e (cref, mref) = do
  sref <- newIORef emptyStats
  pref <- newIORef emptyProfile
  rs <- newIORef noRequests          -- RequestStore
  rq <- newIORef JobNil
  comps <- newTVarIO []              -- completion queue
  return Env
    { cacheRef = cref
    , memoRef = mref
    , flags = defaultFlags
    , userEnv = e
    , states = states
    , statsRef = sref
    , profLabel = "MAIN"
    , profRef = pref
    , reqStoreRef = rs
    , runQueueRef = rq
    , completions = comps
    , pendingWaits = []
    , speculative = 0
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
--  * It is a reader monad for 'Env', which contains the current state
--    of the scheduler, including unfetched requests and the run queue
--    of computations.
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
  { unHaxl :: Env u -> IO (Result u a) }


-- -----------------------------------------------------------------------------
-- JobList

-- | A list of computations together with the IVar into which they
-- should put their result.
--
-- This could be an ordinary list, but the optimised representation
-- saves space and time.
--
data JobList u
 = JobNil
 | forall a . JobCons
     (Env u)          -- See Note [make withEnv work] below.
     (GenHaxl u a)
     {-# UNPACK #-} !(IVar u a)
     (JobList u)

-- Note [make withEnv work]
--
-- The withEnv operation supplies a new Env for the scope of a GenHaxl
-- computation.  The problem is that the computation might be split
-- into pieces and put onto various JobLists, so we have to be sure to
-- use the correct Env when we execute the pieces. Furthermore, if one
-- of these pieces blocks and gets run again later, we must ensure to
-- restart it with the correct Env.  So we stash the Env along with
-- the continuation in the JobList.

appendJobList :: JobList u -> JobList u -> JobList u
appendJobList JobNil c = c
appendJobList c JobNil = c
appendJobList (JobCons a b c d) e = JobCons a b c $! appendJobList d e

lengthJobList :: JobList u -> Int
lengthJobList JobNil = 0
lengthJobList (JobCons _ _ _ j) = 1 + lengthJobList j


-- -----------------------------------------------------------------------------
-- IVar

-- | A synchronisation point.  It either contains a value, or a list
-- of computations waiting for the value.
newtype IVar u a = IVar (IORef (IVarContents u a))

data IVarContents u a
  = IVarFull (ResultVal a)
  | IVarEmpty (JobList u)
    -- morally this is a list of @a -> GenHaxl u ()@, but instead of
    -- using a function, each computation begins with `getIVar` to grab
    -- the value it is waiting for.  This is less type safe but a little
    -- faster (benchmarked with tests/MonadBench.hs).

newIVar :: IO (IVar u a)
newIVar = IVar <$> newIORef (IVarEmpty JobNil)

newFullIVar :: ResultVal a -> IO (IVar u a)
newFullIVar r = IVar <$> newIORef (IVarFull r)

getIVar :: IVar u a -> GenHaxl u a
getIVar (IVar !ref) = GenHaxl $ \_env -> do
  e <- readIORef ref
  case e of
    IVarFull (Ok a) -> return (Done a)
    IVarFull (ThrowHaxl e) -> return (Throw e)
    IVarFull (ThrowIO e) -> throwIO e
    IVarEmpty _ -> return (Blocked (IVar ref) (Cont (getIVar (IVar ref))))

-- Just a specialised version of getIVar, for efficiency in <*>
getIVarApply :: IVar u (a -> b) -> a -> GenHaxl u b
getIVarApply (IVar !ref) a = GenHaxl $ \_env -> do
  e <- readIORef ref
  case e of
    IVarFull (Ok f) -> return (Done (f a))
    IVarFull (ThrowHaxl e) -> return (Throw e)
    IVarFull (ThrowIO e) -> throwIO e
    IVarEmpty _ ->
      return (Blocked (IVar ref) (Cont (getIVarApply (IVar ref) a)))

putIVar :: IVar u a -> ResultVal a -> Env u -> IO ()
putIVar (IVar ref) a Env{..} = do
  e <- readIORef ref
  case e of
    IVarEmpty jobs -> do
      writeIORef ref (IVarFull a)
      modifyIORef' runQueueRef (appendJobList jobs)
    IVarFull{} -> error "putIVar: multiple put"

{-# INLINE addJob #-}
addJob :: Env u -> GenHaxl u b -> IVar u b -> IVar u a -> IO ()
addJob env !haxl !resultIVar (IVar !ref) =
  modifyIORef' ref $ \contents ->
    case contents of
      IVarEmpty list -> IVarEmpty (JobCons env haxl resultIVar list)
      _ -> addJobPanic

addJobPanic :: forall a . a
addJobPanic = error "addJob: not empty"


-- -----------------------------------------------------------------------------
-- ResultVal

-- | The contents of a full IVar.  We have to distinguish exceptions
-- thrown in the IO monad from exceptions thrown in the Haxl monad, so
-- that when the result is fetched using getIVar, we can throw the
-- exception in the right way.
data ResultVal a
  = Ok a
  | ThrowHaxl SomeException
  | ThrowIO SomeException

done :: ResultVal a -> IO (Result u a)
done (Ok a) = return (Done a)
done (ThrowHaxl e) = return (Throw e)
done (ThrowIO e) = throwIO e

eitherToResultThrowIO :: Either SomeException a -> ResultVal a
eitherToResultThrowIO (Right a) = Ok a
eitherToResultThrowIO (Left e)
  | Just HaxlException{} <- fromException e = ThrowHaxl e
  | otherwise = ThrowIO e

eitherToResult :: Either SomeException a -> ResultVal a
eitherToResult (Right a) = Ok a
eitherToResult (Left e) = ThrowHaxl e


-- -----------------------------------------------------------------------------
-- CompleteReq

-- | A completed request from a data source, containing the result,
-- and the 'IVar' representing the blocked computations.  The job of a
-- data source is just to add these to a queue ('completions') using
-- 'putResult'; the scheduler collects them from the queue and unblocks
-- the relevant computations.
data CompleteReq u =
  forall a . CompleteReq (Either SomeException a)
                         !(IVar u a)  -- IVar because the result is cached


-- -----------------------------------------------------------------------------
-- Result

-- | The result of a computation is either 'Done' with a value, 'Throw'
-- with an exception, or 'Blocked' on the result of a data fetch with
-- a continuation.
data Result u a
  = Done a
  | Throw SomeException
  | forall b . Blocked
      {-# UNPACK #-} !(IVar u b)
      (Cont u a)
         -- ^ The 'IVar' is what we are blocked on; 'Cont' is the
         -- continuation.  This might be wrapped further if we're
         -- nested inside multiple '>>=', before finally being added
         -- to the 'IVar'.  Morally @b -> GenHaxl u a@, but see
         -- 'IVar',

instance (Show a) => Show (Result u a) where
  show (Done a) = printf "Done(%s)" $ show a
  show (Throw e) = printf "Throw(%s)" $ show e
  show Blocked{} = "Blocked"

{- Note [Exception]

How do we want to represent Haxl exceptions (those that are thrown by
"throw" in the Haxl monad)?

1) Explicitly via a Throw constructor in the Result type
2) Using throwIO in the IO monad

If we did (2), we would have to use an exception handler in <*>,
because an exception in the right-hand argument of <*> should not
necessarily be thrown by the whole computation - an exception on the
left should get priority, and the left might currently be Blocked.

We must be careful about turning IO monad exceptions into Haxl
exceptions.  An IO monad exception will normally propagate right
out of runHaxl and terminate the whole computation, whereas a Haxl
exception can get dropped on the floor, if it is on the right of
<*> and the left side also throws, for example.  So turning an IO
monad exception into a Haxl exception is a dangerous thing to do.
In particular, we never want to do it for an asynchronous exception
(AllocationLimitExceeded, ThreadKilled, etc.), because these are
supposed to unconditionally terminate the computation.

There are three places where we take an arbitrary IO monad exception and
turn it into a Haxl exception:

 * wrapFetchInCatch.  Here we want to propagate a failure of the
   data source to the callers of the data source, but if the
   failure came from elsewhere (an asynchronous exception), then we
   should just propagate it

 * cacheResult (cache the results of IO operations): again,
   failures of the IO operation should be visible to the caller as
   a Haxl exception, but we exclude asynchronous exceptions from
   this.

 * unsafeToHaxlException: assume the caller knows what they're
   doing, and just wrap all exceptions.
-}


-- -----------------------------------------------------------------------------
-- Cont

-- | A data representation of a Haxl continuation.  This is to avoid
-- repeatedly traversing a left-biased tree in a continuation, leading
-- O(n^2) complexity for some pathalogical cases - see the "seql" benchmark
-- in tests/MonadBench.hs.
-- See "A Smart View on Datatypes", Jaskelioff/Rivas, ICFP'15
data Cont u a
  = Cont (GenHaxl u a)
  | forall b. Cont u b :>>= (b -> GenHaxl u a)
  | forall b. (b -> a) :<$> (Cont u b)

toHaxl :: Cont u a -> GenHaxl u a
toHaxl (Cont haxl) = haxl
toHaxl (m :>>= k) = toHaxlBind m k
toHaxl (f :<$> x) = toHaxlFmap f x

toHaxlBind :: Cont u b -> (b -> GenHaxl u a) -> GenHaxl u a
toHaxlBind (m :>>= k) k2 = toHaxlBind m (k >=> k2)
toHaxlBind (Cont haxl) k = haxl >>= k
toHaxlBind (f :<$> x) k = toHaxlBind x (k . f)

toHaxlFmap :: (a -> b) -> Cont u a -> GenHaxl u b
toHaxlFmap f (m :>>= k) = toHaxlBind m (k >=> return . f)
toHaxlFmap f (Cont haxl) = f <$> haxl
toHaxlFmap f (g :<$> x) = toHaxlFmap (f . g) x

-- -----------------------------------------------------------------------------
-- Monad/Applicative instances

instance Monad (GenHaxl u) where
  return a = GenHaxl $ \_env -> return (Done a)
  GenHaxl m >>= k = GenHaxl $ \env -> do
    e <- m env
    case e of
      Done a -> unHaxl (k a) env
      Throw e -> return (Throw e)
      Blocked ivar cont -> trace_ ">>= Blocked" $
        return (Blocked ivar (cont :>>= k))
  fail msg = GenHaxl $ \_env ->
    return $ Throw $ toException $ MonadFail $ Text.pack msg

  -- We really want the Applicative version of >>
  (>>) = (*>)

instance Functor (GenHaxl u) where
  fmap f (GenHaxl m) = GenHaxl $ \env -> do
    r <- m env
    case r of
      Done a -> return (Done (f a))
      Throw e -> return (Throw e)
      Blocked ivar cont -> trace_ "fmap Blocked" $
        return (Blocked ivar (f :<$> cont))

instance Applicative (GenHaxl u) where
  pure = return
  GenHaxl ff <*> GenHaxl aa = GenHaxl $ \env -> do
    rf <- ff env
    case rf of
      Done f -> do
        ra <- aa env
        case ra of
          Done a -> trace_ "Done/Done" $ return (Done (f a))
          Throw e -> trace_ "Done/Throw" $ return (Throw e)
          Blocked ivar fcont -> trace_ "Done/Blocked" $
            return (Blocked ivar (f :<$> fcont))
      Throw e -> trace_ "Throw" $ return (Throw e)
      Blocked ivar1 fcont -> do
         ra <- aa env
         case ra of
           Done a -> trace_ "Blocked/Done" $
             return (Blocked ivar1 (($ a) :<$> fcont))
           Throw e -> trace_ "Blocked/Throw" $
             return (Blocked ivar1 (fcont :>>= (\_ -> throw e)))
           Blocked ivar2 acont -> trace_ "Blocked/Blocked" $ do
             -- Note [Blocked/Blocked]
              if speculative env /= 0
                then
                  return (Blocked ivar1
                            (Cont (toHaxl fcont <*> toHaxl acont)))
                else do
                  i <- newIVar
                  addJob env (toHaxl fcont) i ivar1
                  let cont = acont :>>= \a -> getIVarApply i a
                  return (Blocked ivar2 cont)

-- Note [Blocked/Blocked]
--
-- This is the tricky case: we're blocked on both sides of the <*>.
-- We need to divide the computation into two pieces that may continue
-- independently when the resources they are blocked on become
-- available.  Moreover, the computation as a whole depends on the two
-- pieces.  It works like this:
--
--   ff <*> aa
--
-- becomes
--
--   (ff >>= putIVar i) <*> (a <- aa; f <- getIVar i; return (f a)
--
-- where the IVar i is a new synchronisation point.  If the right side
-- gets to the `getIVar` first, it will block until the left side has
-- called 'putIVar'.
--
-- We can also do it the other way around:
--
--   (do ff <- f; getIVar i; return (ff a)) <*> (a >>= putIVar i)
--
-- The first was slightly faster according to tests/MonadBench.hs.


-- -----------------------------------------------------------------------------
-- runHaxl

-- | Runs a 'Haxl' computation in the given 'Env'.
runHaxl :: forall u a. Env u -> GenHaxl u a -> IO a
runHaxl env@Env{..} haxl = do

  result@(IVar resultRef) <- newIVar -- where to put the final result
  let
    -- Run a job, and put its result in the given IVar
    schedule :: Env u -> JobList u -> GenHaxl u b -> IVar u b -> IO ()
    schedule env@Env{..} rq (GenHaxl run) (IVar !ref) = do
      ifTrace flags 3 $ printf "schedule: %d\n" (1 + lengthJobList rq)
      let {-# INLINE result #-}
          result r = do
            e <- readIORef ref
            case e of
              IVarFull _ -> error "multiple put"
              IVarEmpty haxls -> do
                writeIORef ref (IVarFull r)
                -- Have we got the final result now?
                if ref == unsafeCoerce resultRef
                        -- comparing IORefs of different types is safe, it's
                        -- pointer-equality on the MutVar#.
                   then
                     -- We have a result, but don't discard unfinished
                     -- computations in the run queue. See
                     -- Note [runHaxl and unfinished requests].
                     -- Nothing can depend on the final IVar, so haxls must
                     -- be empty.
                     case rq of
                       JobNil -> return ()
                       _ -> modifyIORef' runQueueRef (appendJobList rq)
                   else reschedule env (appendJobList haxls rq)
      r <-
        if report flags >= 4          -- withLabel unfolded
          then Exception.try $ profileCont run env
          else Exception.try $ run env
      case r of
        Left e -> do
          rethrowAsyncExceptions e
          result (ThrowIO e)
        Right (Done a) -> result (Ok a)
        Right (Throw ex) -> result (ThrowHaxl ex)
        Right (Blocked ivar fn) -> do
          addJob env (toHaxl fn) (IVar ref) ivar
          reschedule env rq

    -- Here we have a choice:
    --   - If the requestStore is non-empty, we could submit those
    --     requests right away without waiting for more.  This might
    --     be good for latency, especially if the data source doesn't
    --     support batching, or if batching is pessmial.
    --   - To optimise the batch sizes, we want to execute as much as
    --     we can and only submit requests when we have no more
    --     computation to do.
    --   - compromise: wait at least Nms for an outstanding result
    --     before giving up and submitting new requests.
    --
    -- For now we use the batching strategy in the scheduler, but
    -- individual data sources can request that their requests are
    -- sent eagerly by using schedulerHint.
    --
    reschedule :: Env u -> JobList u -> IO ()
    reschedule env@Env{..} haxls = do
      case haxls of
        JobNil -> do
          rq <- readIORef runQueueRef
          case rq of
            JobNil -> emptyRunQueue env
            JobCons env' a b c -> do
              writeIORef runQueueRef JobNil
              schedule env' c a b
        JobCons env' a b c ->
          schedule env' c a b

    emptyRunQueue :: Env u -> IO ()
    emptyRunQueue env@Env{..} = do
      ifTrace flags 3 $ printf "emptyRunQueue\n"
      haxls <- checkCompletions env
      case haxls of
        JobNil -> do
          case pendingWaits of
            [] -> checkRequestStore env
            wait:waits -> do
              ifTrace flags 3 $ printf "invoking wait\n"
              wait
              emptyRunQueue env { pendingWaits = waits } -- check completions
        _ -> reschedule env haxls

    checkRequestStore :: Env u -> IO ()
    checkRequestStore env@Env{..} = do
      reqStore <- readIORef reqStoreRef
      if RequestStore.isEmpty reqStore
        then waitCompletions env
        else do
          writeIORef reqStoreRef noRequests
          (_, waits) <- performRequestStore 0 env reqStore
          ifTrace flags 3 $ printf "performFetches: %d waits\n" (length waits)
          -- empty the cache if we're not caching.  Is this the best
          -- place to do it?  We do get to de-duplicate requests that
          -- happen simultaneously.
          when (caching flags == 0) $
            writeIORef cacheRef emptyDataCache
          emptyRunQueue env{ pendingWaits = waits ++ pendingWaits }

    checkCompletions :: Env u -> IO (JobList u)
    checkCompletions Env{..} = do
      ifTrace flags 3 $ printf "checkCompletions\n"
      comps <- atomically $ do
        c <- readTVar completions
        writeTVar completions []
        return c
      case comps of
        [] -> return JobNil
        _ -> do
          ifTrace flags 3 $ printf "%d complete\n" (length comps)
          let getComplete (CompleteReq a (IVar cr)) = do
                r <- readIORef cr
                case r of
                  IVarFull _ -> do
                    ifTrace flags 3 $ printf "existing result\n"
                    return JobNil
                    -- this happens if a data source reports a result,
                    -- and then throws an exception.  We call putResult
                    -- a second time for the exception, which comes
                    -- ahead of the original request (because it is
                    -- pushed on the front of the completions list) and
                    -- therefore overrides it.
                  IVarEmpty cv -> do
                    writeIORef cr (IVarFull (eitherToResult a))
                    return cv
          jobs <- mapM getComplete comps
          return (foldr appendJobList JobNil jobs)

    waitCompletions :: Env u -> IO ()
    waitCompletions env@Env{..} = do
      ifTrace flags 3 $ printf "waitCompletions\n"
      atomically $ do
        c <- readTVar completions
        when (null c) retry
      emptyRunQueue env

  --
  schedule env JobNil haxl result
  r <- readIORef resultRef
  case r of
    IVarEmpty _ -> throwIO (CriticalError "runHaxl: missing result")
    IVarFull (Ok a)  -> return a
    IVarFull (ThrowHaxl e)  -> throwIO e
    IVarFull (ThrowIO e)  -> throwIO e


{- Note [runHaxl and unfinished requests]

runHaxl returns immediately when the supplied computation has returned
a result.  This doesn't necessarily mean that the whole computaiton
graph has completed, however.  In particular, when using pAnd and pOr,
we might have created some data fetches that have not completed, but
weren't required, because the other branch of the pAnd/pOr subsumed
the result.

When runHaxl returns, it might be that:
- reqStoreRef contains some unsubmitted requests
- runQueueRef contains some jobs
- there are in-flight BackgroundFetch requests, that will return their
  results to the completions queue in due course.
- there are various unfilled IVars in the cache and/or memo tables

This should be all safe, we can even restart runHaxl with the same Env
after it has stopped and the in-progress computations will
continue. But don't discard the contents of
reqStoreRef/runQueueRef/completions, because then we'll deadlock if we
discover one of the unfilled IVars in the cache or memo table.
-}

{- TODO: later
data SchedPolicy
  = SubmitImmediately
  | WaitAtLeast Int{-ms-}
  | WaitForAllPendingRequests
-}


-- -----------------------------------------------------------------------------
-- Env utils

-- | Extracts data from the 'Env'.
env :: (Env u -> a) -> GenHaxl u a
env f = GenHaxl $ \env -> return (Done (f env))

-- | Returns a version of the Haxl computation which always uses the
-- provided 'Env', ignoring the one specified by 'runHaxl'.
withEnv :: Env u -> GenHaxl u a -> GenHaxl u a
withEnv newEnv (GenHaxl m) = GenHaxl $ \_env -> do
  r <- m newEnv
  case r of
    Done a -> return (Done a)
    Throw e -> return (Throw e)
    Blocked ivar k ->
      return (Blocked ivar (Cont (withEnv newEnv (toHaxl k))))


-- -----------------------------------------------------------------------------
-- Profiling

-- | Label a computation so profiling data is attributed to the label.
withLabel :: ProfileLabel -> GenHaxl u a -> GenHaxl u a
withLabel l (GenHaxl m) = GenHaxl $ \env ->
  if report (flags env) < 4
     then m env
     else collectProfileData l m env

-- | Label a computation so profiling data is attributed to the label.
-- Intended only for internal use by 'memoFingerprint'.
withFingerprintLabel :: Addr# -> Addr# -> GenHaxl u a -> GenHaxl u a
withFingerprintLabel mnPtr nPtr (GenHaxl m) = GenHaxl $ \env ->
  if report (flags env) < 4
     then m env
     else collectProfileData
            (Text.unpackCString# mnPtr <> "." <> Text.unpackCString# nPtr)
            m env

-- | Collect profiling data and attribute it to given label.
collectProfileData
  :: ProfileLabel
  -> (Env u -> IO (Result u a))
  -> Env u
  -> IO (Result u a)
collectProfileData l m env = do
   a0 <- getAllocationCounter
   r <- m env{profLabel=l} -- what if it throws?
   a1 <- getAllocationCounter
   modifyProfileData env l (a0 - a1)
   -- So we do not count the allocation overhead of modifyProfileData
   setAllocationCounter a1
   case r of
     Done a -> return (Done a)
     Throw e -> return (Throw e)
     Blocked ivar k -> return (Blocked ivar (Cont (withLabel l (toHaxl k))))
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


-- Like collectProfileData, but intended to be run from the scheduler.
--
-- * doesn't add a dependency (the original withLabel did this)
--
-- * doesn't subtract allocs from the caller (we're evaluating this
--   cont from the top level, so we don't need this)
--
-- * doesn't wrap a Blocked continuation in withLabel (the scheduler
--   will call profileCont the next time this cont runs)
--
profileCont
  :: (Env u -> IO (Result u a))
  -> Env u
  -> IO (Result u a)
profileCont m env = do
  a0 <- getAllocationCounter
  r <- m env
  a1 <- getAllocationCounter
  let
    allocs = a0 - a1
    newEntry = emptyProfileData { profileAllocs = allocs }
    updEntry _ old = old { profileAllocs = profileAllocs old + allocs }
  modifyIORef' (profRef env) $ \ p ->
    p { profile =
         HashMap.insertWith updEntry (profLabel env) newEntry $
         profile p }
  -- So we do not count the allocation overhead of modifyProfileData
  setAllocationCounter a1
  return r
{-# INLINE profileCont #-}

-- -----------------------------------------------------------------------------
-- Exceptions

-- | Throw an exception in the Haxl monad
throw :: (Exception e) => e -> GenHaxl u a
throw e = GenHaxl $ \_env -> raise e

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
catch (GenHaxl m) h = GenHaxl $ \env -> do
   r <- m env
   case r of
     Done a    -> return (Done a)
     Throw e | Just e' <- fromException e -> unHaxl (h e') env
             | otherwise -> return (Throw e)
     Blocked ivar k -> return (Blocked ivar (Cont (catch (toHaxl k) h)))

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
unsafeLiftIO m = GenHaxl $ \_env -> Done <$> m

-- | Convert exceptions in the underlying IO monad to exceptions in
-- the Haxl monad.  This is morally unsafe, because you could then
-- catch those exceptions in Haxl and observe the underlying execution
-- order.  Not to be exposed to user code.
unsafeToHaxlException :: GenHaxl u a -> GenHaxl u a
unsafeToHaxlException (GenHaxl m) = GenHaxl $ \env -> do
  r <- m env `Exception.catch` \e -> return (Throw e)
  case r of
    Blocked cvar c ->
      return (Blocked cvar (Cont (unsafeToHaxlException (toHaxl c))))
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
data CacheResult u a
  -- | The request hadn't been seen until now.
  = Uncached
       (ResultVar a)
       {-# UNPACK #-} !(IVar u a)

  -- | The request has been seen before, but its result has not yet been
  -- fetched.
  | CachedNotFetched
      {-# UNPACK #-} !(IVar u a)

  -- | The request has been seen before, and its result has already been
  -- fetched.
  | Cached (ResultVal a)


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
  :: forall r a u .
     Typeable (r a)
  => (r a -> String)    -- See Note [showFn]
  -> (r a -> IVar u a -> DataCache (IVar u) -> DataCache (IVar u))
  -> Env u -> r a -> IO (CacheResult u a)
cachedWithInsert showFn insertFn Env{..} req = do
  cache <- readIORef cacheRef
  let
    doFetch = do
      ivar <- newIVar
      let done r = atomically $ do
            cs <- readTVar completions
            writeTVar completions (CompleteReq r ivar : cs)
      writeIORef cacheRef $! insertFn req ivar cache
      return (Uncached (mkResultVar done) ivar)
  case DataCache.lookup req cache of
    Nothing -> doFetch
    Just (IVar cr) -> do
      e <- readIORef cr
      case e of
        IVarEmpty _ -> return (CachedNotFetched (IVar cr))
        IVarFull r -> do
          ifTrace flags 3 $ putStrLn $ case r of
            ThrowIO _ -> "Cached error: " ++ showFn req
            ThrowHaxl _ -> "Cached error: " ++ showFn req
            Ok _ -> "Cached request: " ++ showFn req
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
  :: forall u r a
   . (DataSource u r, Eq (r a), Hashable (r a), Typeable (r a))
  => (r a -> String)    -- See Note [showFn]
  -> (r a -> IVar u a -> DataCache (IVar u) -> DataCache (IVar u))
  -> r a
  -> GenHaxl u a
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
      return $ Blocked ivar (Cont (getIVar ivar))

    -- Seen before but not fetched yet.  We're blocked, but we don't have
    -- to add the request to the RequestStore.
    CachedNotFetched ivar -> return $ Blocked ivar (Cont (getIVar ivar))

    -- Cached: either a result, or an exception
    Cached r -> done r

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

      upd :: ProfileData -> ProfileData
      upd d = d { profileFetches =
        HashMap.insertWith (+) dsName 1 (profileFetches d) }

    in p { profile = HashMap.adjust upd (profLabel env) (profile p) }
  -- So we do not count the allocation overhead of addProfileFetch
  setAllocationCounter c


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
uncachedRequest :: (DataSource u r, Show (r a)) => r a -> GenHaxl u a
uncachedRequest req = GenHaxl $ \Env{..} -> do
  cr <- newIVar
  let done r = atomically $ do
        cs <- readTVar completions
        writeTVar completions (CompleteReq r cr : cs)
  modifyIORef' reqStoreRef $ \bs ->
    addRequest (BlockedFetch req (mkResultVar done)) bs
  return $ Blocked cr (Cont (getIVar cr))


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
  -> (r a -> IVar u a -> DataCache (IVar u) -> DataCache (IVar u)) -> r a
  -> IO a -> GenHaxl u a
cacheResultWithInsert showFn insertFn req val = GenHaxl $ \env -> do
  let !ref = cacheRef env
  cache <- readIORef ref
  case DataCache.lookup req cache of
    Nothing -> do
      eitherResult <- Exception.try val
      case eitherResult of
        Left e -> rethrowAsyncExceptions e
        _ -> return ()
      let result = eitherToResultThrowIO eitherResult
      ivar <- newFullIVar result
      writeIORef ref $! insertFn req ivar cache
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
  :: Request req a => req a -> Either SomeException a -> GenHaxl u ()
cacheRequest request result = GenHaxl $ \env -> do
  cache <- readIORef (cacheRef env)
  case DataCache.lookup request cache of
    Nothing -> do
      cr <- newFullIVar (eitherToResult result)
      writeIORef (cacheRef env) $! DataCache.insert request cr cache
      return (Done ())

    -- It is an error if the request is already in the cache.  We can't test
    -- whether the cached result is the same without adding an Eq constraint,
    -- and we don't necessarily have Eq for all results.
    _other -> raise $
      DataSourceError "cacheRequest: request is already in the cache"

instance IsString a => IsString (GenHaxl u a) where
  fromString s = return (fromString s)

performRequestStore
   :: forall u. Int -> Env u -> RequestStore u -> IO (Int, [IO ()])
performRequestStore n env reqStore =
  performFetches n env (contents reqStore)

-- | Issues a batch of fetches in a 'RequestStore'. After
-- 'performFetches', all the requests in the 'RequestStore' are
-- complete, and all of the 'ResultVar's are full.
performFetches
  :: forall u. Int -> Env u -> [BlockedFetches u] -> IO (Int, [IO ()])
performFetches n env@Env{flags=f, statsRef=sref} jobs = do
  let !n' = n + length jobs

  t0 <- getTimestamp

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
    applyFetch i (BlockedFetches (reqs :: [BlockedFetch r])) =
      case stateGet (states env) of
        Nothing ->
          return (FetchToDo reqs (SyncFetch (mapM_ (setError e))))
         where
           e req = DataSourceError $ "data source not initialized: " <> dsName
                  <> ": "
                  <> Text.pack (showp req)
        Just state ->
          return
            $ FetchToDo reqs
            $ (if report f >= 2
                then wrapFetchInStats sref dsName (length reqs)
                else id)
            $ wrapFetchInTrace i (length reqs)
               (dataSourceName (undefined :: r a))
            $ wrapFetchInCatch reqs
            $ fetch state f (userEnv env)
      where
        req :: r a; req = undefined; dsName = dataSourceName req

  fetches <- zipWithM applyFetch [n..] jobs

  waits <- scheduleFetches fetches

  t1 <- getTimestamp
  let roundtime = fromIntegral (t1 - t0) / 1000000 :: Double

  ifTrace f 1 $
    printf "Batch data fetch done (%.2fs)\n" (realToFrac roundtime :: Double)

  return (n', waits)

data FetchToDo where
  FetchToDo :: [BlockedFetch req] -> PerformFetch req -> FetchToDo

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
      BlockedFetch req $ ResultVar $ \result -> do
        t1 <- getTimestamp
        updateFetchStats t0 (t1 - t0)
          0 -- allocs: we can't measure this easily for BackgroundFetch
          1 -- batch size: we don't know if this is a batch or not
          (if isLeft result then 1 else 0) -- failures
        fn result

    updateFetchStats :: Timestamp -> Microseconds -> Int64 -> Int -> Int -> IO ()
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
      BlockedFetch req $ ResultVar $ \result -> do
        when (isLeft result) $ atomicModifyIORef' ref (\r -> (r+1,()))
        fn result

wrapFetchInTrace
  :: Int
  -> Int
  -> Text.Text
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
scheduleFetches :: [FetchToDo] -> IO [IO ()]
scheduleFetches fetches = do
  fully_async_fetches
  waits <- future_fetches
  async_fetches sync_fetches
  return waits
 where
  fully_async_fetches :: IO ()
  fully_async_fetches =
    sequence_ [f reqs | FetchToDo reqs (BackgroundFetch f) <- fetches]

  future_fetches :: IO [IO ()]
  future_fetches = sequence [f reqs | FetchToDo reqs (FutureFetch f) <- fetches]

  async_fetches :: IO () -> IO ()
  async_fetches = compose [f reqs | FetchToDo reqs (AsyncFetch f) <- fetches]

  sync_fetches :: IO ()
  sync_fetches = sequence_ [f reqs | FetchToDo reqs (SyncFetch f) <- fetches]


-- -----------------------------------------------------------------------------
-- Memoization

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

cachedComputation req haxl = GenHaxl $ \env@Env{..} -> do
  cache <- readIORef memoRef
  ifProfiling flags $
     modifyIORef' profRef (incrementMemoHitCounterFor profLabel)
  case DataCache.lookup req cache of
    Just ivar -> unHaxl (getIVar ivar) env
    Nothing -> do
      ivar <- newIVar
      writeIORef memoRef $! DataCache.insertNotShowable req ivar cache
      unHaxl (execMemoNow haxl ivar) env


-- | Like 'cachedComputation', but fails if the cache is already
-- populated.
--
-- Memoization can be (ab)used to "mock" a cached computation, by
-- pre-populating the cache with an alternative implementation. In
-- that case we don't want the operation to populate the cache to
-- silently succeed if the cache is already populated.
--
preCacheComputation
  :: forall req u a.
     ( Eq (req a)
     , Hashable (req a)
     , Typeable (req a))
  => req a -> GenHaxl u a -> GenHaxl u a
preCacheComputation req haxl = GenHaxl $ \env@Env{..} -> do
  cache <- readIORef memoRef
  ifProfiling flags $
     modifyIORef' profRef (incrementMemoHitCounterFor profLabel)
  case DataCache.lookup req cache of
    Just _ -> return $ Throw $ toException $ InvalidParameter $
      "preCacheComputation: key is already cached"
    Nothing -> do
      ivar <- newIVar
      writeIORef memoRef $! DataCache.insertNotShowable req ivar cache
      unHaxl (execMemoNow haxl ivar) env

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
  let
    readIVar (IVar ref) = do
      r <- readIORef ref
      case r of
        IVarFull (Ok a) -> return (Just (Right a))
        IVarFull (ThrowHaxl e) -> return (Just (Left e))
        IVarFull (ThrowIO e) -> return (Just (Left e))
        IVarEmpty _ -> return Nothing

    mk_cr (req, res) =
      text "cacheRequest" <+> parens (text req) <+> parens (result res)
    result (Left e) = text "except" <+> parens (text (show e))
    result (Right s) = text "Right" <+> parens (text s)

  entries <- unsafeLiftIO $ do
    cache <- readIORef ref
    showCache cache readIVar

  return $ show $
    text (fnName ++ " :: " ++ fnType) $$
    text (fnName ++ " = do") $$
      nest 2 (vcat (map mk_cr (concatMap snd entries))) $$
    text "" -- final newline

-- -----------------------------------------------------------------------------
-- Memoization

newtype MemoVar u a = MemoVar (IORef (MemoStatus u a))

data MemoStatus u a
  = MemoEmpty
  | MemoReady (GenHaxl u a)
  | MemoRun {-# UNPACK #-} !(IVar u a)

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
  = unsafeLiftIO $ writeIORef memoRef (MemoReady memoCmp)

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
runMemo (MemoVar memoRef) = GenHaxl $ \env -> do
  stored <- readIORef memoRef
  case stored of
    -- Memo was not prepared first; throw an exception.
    MemoEmpty -> raise $ CriticalError "Attempting to run empty memo."
    -- Memo has been prepared but not run yet
    MemoReady cont -> do
      ivar <- newIVar
      writeIORef memoRef (MemoRun ivar)
      unHaxl (execMemoNow cont ivar) env
    -- The memo has already been run, get (or wait for) for the result
    MemoRun ivar -> unHaxl (getIVar ivar) env


execMemoNow :: GenHaxl u a -> IVar u a -> GenHaxl u a
execMemoNow cont ivar = GenHaxl $ \env -> do
  let !ienv = imperative env   -- don't speculate under memoized things
  r <- Exception.try $ unHaxl cont ienv
  case r of
    Left e -> do
      rethrowAsyncExceptions e
      putIVar ivar (ThrowIO e) env
      throwIO e
    Right (Done a) -> do
      putIVar ivar (Ok a) env
      return (Done a)
    Right (Throw ex) -> do
      putIVar ivar (ThrowHaxl ex) env
      return (Throw ex)
    Right (Blocked ivar' cont) -> do
      addJob env (toHaxl cont) ivar ivar'
      return (Blocked ivar (Cont (getIVar ivar)))

-- -----------------------------------------------------------------------------
-- 1-ary and 2-ary memo functions

newtype MemoVar1 u a b = MemoVar1 (IORef (MemoStatus1 u a b))
newtype MemoVar2 u a b c = MemoVar2 (IORef (MemoStatus2 u a b c))

data MemoStatus1 u a b
  = MemoEmpty1
  | MemoTbl1 (a -> GenHaxl u b) (HashMap.HashMap a (MemoVar u b))

data MemoStatus2 u a b c
  = MemoEmpty2
  | MemoTbl2
      (a -> b -> GenHaxl u c)
      (HashMap.HashMap a (HashMap.HashMap b (MemoVar u c)))

newMemo1 :: GenHaxl u (MemoVar1 u a b)
newMemo1 = unsafeLiftIO $ MemoVar1 <$> newIORef MemoEmpty1

newMemoWith1 :: (a -> GenHaxl u b) -> GenHaxl u (MemoVar1 u a b)
newMemoWith1 f = newMemo1 >>= \r -> prepareMemo1 r f >> return r

prepareMemo1 :: MemoVar1 u a b -> (a -> GenHaxl u b) -> GenHaxl u ()
prepareMemo1 (MemoVar1 r) f
  = unsafeLiftIO $ writeIORef r (MemoTbl1 f HashMap.empty)

runMemo1 :: (Eq a, Hashable a) => MemoVar1 u a b -> a -> GenHaxl u b
runMemo1 (MemoVar1 r) k = unsafeLiftIO (readIORef r) >>= \case
  MemoEmpty1 -> throw $ CriticalError "Attempting to run empty memo."
  MemoTbl1 f h -> case HashMap.lookup k h of
    Nothing -> do
      x <- newMemoWith (f k)
      unsafeLiftIO $ writeIORef r (MemoTbl1 f (HashMap.insert k x h))
      runMemo x
    Just v -> runMemo v

newMemo2 :: GenHaxl u (MemoVar2 u a b c)
newMemo2 = unsafeLiftIO $ MemoVar2 <$> newIORef MemoEmpty2

newMemoWith2 :: (a -> b -> GenHaxl u c) -> GenHaxl u (MemoVar2 u a b c)
newMemoWith2 f = newMemo2 >>= \r -> prepareMemo2 r f >> return r

prepareMemo2 :: MemoVar2 u a b c -> (a -> b -> GenHaxl u c) -> GenHaxl u ()
prepareMemo2 (MemoVar2 r) f
  = unsafeLiftIO $ writeIORef r (MemoTbl2 f HashMap.empty)

runMemo2 :: (Eq a, Hashable a, Eq b, Hashable b)
         => MemoVar2 u a b c
         -> a -> b -> GenHaxl u c
runMemo2 (MemoVar2 r) k1 k2 = unsafeLiftIO (readIORef r) >>= \case
  MemoEmpty2 -> throw $ CriticalError "Attempting to run empty memo."
  MemoTbl2 f h1 -> case HashMap.lookup k1 h1 of
    Nothing -> do
      v <- newMemoWith (f k1 k2)
      unsafeLiftIO $ writeIORef r
        (MemoTbl2 f (HashMap.insert k1 (HashMap.singleton k2 v) h1))
      runMemo v
    Just h2 -> case HashMap.lookup k2 h2 of
      Nothing -> do
        v <- newMemoWith (f k1 k2)
        unsafeLiftIO $ writeIORef r
          (MemoTbl2 f (HashMap.insert k1 (HashMap.insert k2 v h2) h1))
        runMemo v
      Just v -> runMemo v



-- -----------------------------------------------------------------------------
-- Parallel operations

-- Bind more tightly than .&&, .||
infixr 5 `pAnd`
infixr 4 `pOr`

speculate :: Env u -> Env u
speculate env@Env{..}
  | speculative == 0 = env { speculative = 1 }
  | otherwise = env

imperative :: Env u -> Env u
imperative env@Env{..}
  | speculative == 1 = env { speculative = 0 }
  | otherwise = env

-- | Parallel version of '(.||)'.  Both arguments are evaluated in
-- parallel, and if either returns 'True' then the other is
-- not evaluated any further.
--
-- WARNING: exceptions may be unpredictable when using 'pOr'.  If one
-- argument returns 'True' before the other completes, then 'pOr'
-- returns 'True' immediately, ignoring a possible exception that
-- the other argument may have produced if it had been allowed to
-- complete.
pOr :: GenHaxl u Bool -> GenHaxl u Bool -> GenHaxl u Bool
GenHaxl a `pOr` GenHaxl b = GenHaxl $ \env@Env{..} -> do
  let !senv = speculate env
  ra <- a senv
  case ra of
    Done True -> return (Done True)
    Done False -> b env  -- not speculative
    Throw _ -> return ra
    Blocked ia a' -> do
      rb <- b senv
      case rb of
        Done True -> return rb
        Done False -> return ra
        Throw _ -> return rb
        Blocked _ b' -> return (Blocked ia (Cont (toHaxl a' `pOr` toHaxl b')))
          -- Note [pOr Blocked/Blocked]
          -- This will only wake up when ia is filled, which
          -- is whatever the left side was waiting for.  This is
          -- suboptimal because the right side might wake up first,
          -- but handling this non-determinism would involve a much
          -- more complicated implementation here.

-- | Parallel version of '(.&&)'.  Both arguments are evaluated in
-- parallel, and if either returns 'False' then the other is
-- not evaluated any further.
--
-- WARNING: exceptions may be unpredictable when using 'pAnd'.  If one
-- argument returns 'False' before the other completes, then 'pAnd'
-- returns 'False' immediately, ignoring a possible exception that
-- the other argument may have produced if it had been allowed to
-- complete.
pAnd :: GenHaxl u Bool -> GenHaxl u Bool -> GenHaxl u Bool
GenHaxl a `pAnd` GenHaxl b = GenHaxl $ \env@Env{..} -> do
  let !senv = speculate env
  ra <- a senv
  case ra of
    Done False -> return (Done False)
    Done True -> b env
    Throw _ -> return ra
    Blocked ia a' -> do
      rb <- b senv
      case rb of
        Done False -> return rb
        Done True -> return ra
        Throw _ -> return rb
        Blocked _ b' -> return (Blocked ia (Cont (toHaxl a' `pAnd` toHaxl b')))
         -- See Note [pOr Blocked/Blocked]
