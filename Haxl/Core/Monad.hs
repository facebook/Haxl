-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

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
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- The implementation of the 'Haxl' monad.  Most users should
-- import "Haxl.Core" instead of importing this module directly.
--
module Haxl.Core.Monad
  (
    -- * The monad
    GenHaxl(..)
  , Result(..)

    -- * Writes (for debugging only)
  , WriteTree(..)
  , tellWrite
  , tellWriteNoMemo
  , write
  , writeNoMemo
  , flattenWT
  , appendWTs
  , mbModifyWLRef

    -- * Cont
  , Cont(..)
  , toHaxl

    -- * IVar
  , IVar(..)
  , IVarContents(..)
  , newIVar
  , newFullIVar
  , withCurrentCCS
  , getIVar
  , getIVarWithWrites
  , putIVar

    -- * ResultVal
  , ResultVal(..)
  , done
  , eitherToResult
  , eitherToResultThrowIO

    -- * CompleteReq
  , CompleteReq(..)

    -- * Env
  , Env(..)
  , DataCacheItem(..)
  , Caches
  , caches
  , initEnvWithData
  , initEnv
  , emptyEnv
  , env, withEnv
  , nextCallId
  , speculate
  , imperative

    -- * Profiling
  , ProfileCurrent(..)

    -- * JobList
  , JobList(..)
  , appendJobList
  , lengthJobList
  , addJob

    -- * Exceptions
  , throw
  , raise
  , catch
  , catchIf
  , try
  , tryToHaxlException

    -- * Dumping the cache
  , dumpCacheAsHaskell
  , dumpCacheAsHaskellFn

    -- * CallGraph
#ifdef PROFILING
  , withCallGraph
#endif

    -- * Unsafe operations
  ,  unsafeLiftIO, unsafeToHaxlException
  ) where

import Haxl.Core.Flags
import Haxl.Core.Stats
import Haxl.Core.StateStore
import Haxl.Core.Exception
import Haxl.Core.RequestStore as RequestStore
import Haxl.Core.DataCache as DataCache
import Haxl.Core.Util (trace_)

import Control.Arrow (left)
import Control.Concurrent.STM
import qualified Data.Text as Text
import qualified Control.Monad.Catch as Catch
import Control.Exception (Exception(..), SomeException, throwIO)
import Control.Monad
import qualified Control.Exception as Exception
import Data.IORef
import Data.Int
import Data.Either (rights)
import GHC.Exts (IsString(..))
import Text.PrettyPrint hiding ((<>))
import Text.Printf
#ifdef EVENTLOG
import Control.Exception (bracket_)
import Debug.Trace (traceEventIO)
#endif

#ifdef PROFILING
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Typeable
import Foreign.Ptr (Ptr)
import GHC.Stack
import Haxl.Core.CallGraph
#endif

-- -----------------------------------------------------------------------------
-- The environment

-- | The data we carry around in the Haxl monad.

data DataCacheItem u w a = DataCacheItem (IVar u w a) {-# UNPACK #-} !CallId

data Env u w = Env
  { dataCache     :: {-# UNPACK #-} !(DataCache (DataCacheItem u w))
      -- ^ cached data fetches

  , memoCache      :: {-# UNPACK #-} !(DataCache (DataCacheItem u w))
      -- ^ memoized computations

  , memoKey    :: {-# UNPACK #-} !CallId
      -- ^ current running memo key

  , flags        :: !Flags
      -- conservatively not unpacking, because this is passed
      -- to 'fetch' and would need to be rebuilt.

  , userEnv      :: u
      -- ^ user-supplied data, retrievable with 'env'

  , statsRef     :: {-# UNPACK #-} !(IORef Stats)
      -- ^ statistics, collected according to the 'report' level in 'flags'.

  , statsBatchIdRef :: {-# UNPACK #-} !(IORef Int)
     -- ^ keeps track of a Unique ID for each batch dispatched with stats
     -- enabled, for aggregating after.

  , callIdRef :: {-# UNPACK #-} !(IORef CallId)
     -- ^ keeps track of a Unique ID for each fetch/memo.

  , profCurrent    :: ProfileCurrent
     -- ^ current profiling label, see 'withLabel'

  , profRef      :: {-# UNPACK #-} !(IORef Profile)
      -- ^ profiling data, collected according to the 'report' level in 'flags'.

  , states       :: StateStore
      -- ^ Data sources and other components can store their state in
      -- here. Items in this store must be instances of 'StateKey'.

  , reqStoreRef :: {-# UNPACK #-} !(IORef (RequestStore u))
       -- ^ The set of requests that we have not submitted to data sources yet.
       -- Owned by the scheduler.

  , runQueueRef :: {-# UNPACK #-} !(IORef (JobList u w))
       -- ^ runnable computations. Things get added to here when we wake up
       -- a computation that was waiting for something.  When the list is
       -- empty, either we're finished, or we're waiting for some data fetch
       -- to return.

  , submittedReqsRef :: {-# UNPACK #-} !(IORef ReqCountMap)
       -- ^ all outgone fetches which haven't yet returned. Entries are
       -- removed from this map as the fetches finish. This field is
       -- useful for tracking outgone fetches to detect downstream
       -- failures.

  , completions :: {-# UNPACK #-} !(TVar [CompleteReq u w])
       -- ^ Requests that have completed.  Modified by data sources
       -- (via putResult) and the scheduler.  Waiting for this list to
       -- become non-empty is how the scheduler blocks waiting for
       -- data fetches to return.

  , speculative :: {-# UNPACK #-} !Int

  , writeLogsRef :: {-# UNPACK #-} !(IORef (WriteTree w))
       -- ^ A log of all writes done as part of this haxl computation. Any
       -- haxl computation that needs to be memoized runs in its own
       -- environment so that we can get a hold of those writes and put them
       -- in the IVar associated with the compuatation.
  , writeLogsRefNoMemo :: {-# UNPACK #-} !(IORef (WriteTree w))
       -- ^ This is just a specialized version of @writeLogsRef@, where we put
       -- logs that user doesn't want memoized. This is a better alternative to
       -- doing arbitrary IO from a (memoized) Haxl computation.
#ifdef PROFILING
  , callGraphRef ::  Maybe (IORef CallGraph)
       -- ^ An edge list representing the current function call graph. The type
       -- is wrapped in a Maybe to avoid changing the existing callsites.

  , currFunction :: QualFunction
       -- ^ The most recent function call.
#endif
  }

data ProfileCurrent = ProfileCurrent
  { profCurrentKey ::  {-# UNPACK #-} !ProfileKey
  , profCurrentLabel :: {-# UNPACK #-} !ProfileLabel
  }

type Caches u w = (DataCache (DataCacheItem u w), DataCache (DataCacheItem u w))

caches :: Env u w -> Caches u w
caches env = (dataCache env, memoCache env)

getMaxCallId :: DataCache (DataCacheItem u w) -> IO (Maybe Int)
getMaxCallId c = do
  callIds  <- rights . concatMap snd <$>
              DataCache.readCache c (\(DataCacheItem _ i) -> return i)
  case callIds of
    [] -> return Nothing
    vals -> return $ Just (maximum vals)


-- | Initialize an environment with a 'StateStore', an input map, a
-- preexisting 'DataCache', and a seed for the random number generator.
initEnvWithData :: StateStore -> u -> Caches u w -> IO (Env u w)
initEnvWithData states e (dcache, mcache) = do
  newCid <- max <$>
    (maybe 0 ((+) 1) <$> getMaxCallId dcache) <*>
    (maybe 0 ((+) 1) <$> getMaxCallId mcache)
  ciref<- newIORef newCid
  sref <- newIORef emptyStats
  sbref <- newIORef 0
  pref <- newIORef emptyProfile
  rs <- newIORef noRequests          -- RequestStore
  rq <- newIORef JobNil
  sr <- newIORef emptyReqCounts
  comps <- newTVarIO []              -- completion queue
  wl <- newIORef NilWrites
  wlnm <- newIORef NilWrites
  return Env
    { dataCache = dcache
    , memoCache = mcache
    , memoKey = (-1)
    , flags = defaultFlags
    , userEnv = e
    , states = states
    , statsRef = sref
    , statsBatchIdRef = sbref
    , profCurrent = ProfileCurrent 0 "MAIN"
    , callIdRef = ciref
    , profRef = pref
    , reqStoreRef = rs
    , runQueueRef = rq
    , submittedReqsRef = sr
    , completions = comps
    , speculative = 0
    , writeLogsRef = wl
    , writeLogsRefNoMemo = wlnm
#ifdef PROFILING
    , callGraphRef = Nothing
    , currFunction = mainFunction
#endif
    }

-- | Initializes an environment with 'StateStore' and an input map.
initEnv :: StateStore -> u -> IO (Env u w)
initEnv states e = do
  dcache <- emptyDataCache
  mcache <- emptyDataCache
  initEnvWithData states e (dcache, mcache)

-- | A new, empty environment.
emptyEnv :: u -> IO (Env u w)
emptyEnv = initEnv stateEmpty

speculate :: Env u w -> Env u w
speculate env@Env{..}
  | speculative == 0 = env { speculative = 1 }
  | otherwise = env

imperative :: Env u w -> Env u w
imperative env@Env{..}
  | speculative == 1 = env { speculative = 0 }
  | otherwise = env

-- -----------------------------------------------------------------------------
-- WriteTree

-- | A tree of writes done during a Haxl computation. We could use a simple
-- list, but this allows us to avoid multiple mappends when concatenating
-- writes from two haxl computations.
--
-- Users should try to treat this data type as opaque, and prefer
-- to use @flattenWT@ to get a simple list of writes from a @WriteTree@.
data WriteTree w
  = NilWrites
  | SomeWrite w
  | MergeWrites (WriteTree w) (WriteTree w)
  deriving (Show)

appendWTs :: WriteTree w -> WriteTree w -> WriteTree w
appendWTs NilWrites w = w
appendWTs w NilWrites = w
appendWTs w1 w2 = MergeWrites w1 w2

-- This function must be called at the end of the Haxl computation to get
-- a list of writes.
-- Haxl provides no guarantees on the order of the returned logs.
flattenWT :: WriteTree w -> [w]
flattenWT = go []
  where
    go !ws NilWrites = ws
    go !ws (SomeWrite w) = w : ws
    go !ws (MergeWrites w1 w2) = go (go ws w2) w1

-- This is a convenience wrapper over modifyIORef, which only modifies
-- writeLogsRef IORef, for non NilWrites.
mbModifyWLRef :: WriteTree w -> IORef (WriteTree w) -> IO ()
mbModifyWLRef NilWrites _ = return ()
mbModifyWLRef !wt ref = modifyIORef' ref (`appendWTs` wt)

-- -----------------------------------------------------------------------------
-- | The Haxl monad, which does several things:
--
--  * It is a reader monad for 'Env', which contains the current state
--    of the scheduler, including unfetched requests and the run queue
--    of computations.
--
--  * It is a writer monad for 'WriteTree'. These can be used to do
--    arbitrary "logs" from any Haxl computation. These are better than
--    doing arbitrary IO from a Haxl computation as these writes also get
--    memoized if the Haxl computation associated with them is memoized.
--    Now if this memoized computation is run again, you'll get the writes
--    twice.

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
newtype GenHaxl u w a = GenHaxl
  { unHaxl :: Env u w -> IO (Result u w a) }

tellWrite :: w -> GenHaxl u w ()
tellWrite = write . SomeWrite

write :: WriteTree w -> GenHaxl u w ()
write wt = GenHaxl $ \Env{..} -> do
  mbModifyWLRef wt writeLogsRef
  return $ Done ()

tellWriteNoMemo :: w -> GenHaxl u w ()
tellWriteNoMemo = writeNoMemo . SomeWrite

writeNoMemo :: WriteTree w -> GenHaxl u w ()
writeNoMemo wt = GenHaxl $ \Env{..} -> do
  mbModifyWLRef wt writeLogsRefNoMemo
  return $ Done ()


instance IsString a => IsString (GenHaxl u w a) where
  fromString s = return (fromString s)

-- -----------------------------------------------------------------------------
-- JobList

-- | A list of computations together with the IVar into which they
-- should put their result.
--
-- This could be an ordinary list, but the optimised representation
-- saves space and time.
--
data JobList u w
 = JobNil
 | forall a . JobCons
     (Env u w)          -- See Note [make withEnv work] below.
     (GenHaxl u w a)
     {-# UNPACK #-} !(IVar u w a)
     (JobList u w)

-- Note [make withEnv work]
--
-- The withEnv operation supplies a new Env for the scope of a GenHaxl
-- computation.  The problem is that the computation might be split
-- into pieces and put onto various JobLists, so we have to be sure to
-- use the correct Env when we execute the pieces. Furthermore, if one
-- of these pieces blocks and gets run again later, we must ensure to
-- restart it with the correct Env.  So we stash the Env along with
-- the continuation in the JobList.

appendJobList :: JobList u w -> JobList u w -> JobList u w
appendJobList JobNil c = c
appendJobList c JobNil = c
appendJobList (JobCons a b c d) e = JobCons a b c $! appendJobList d e

lengthJobList :: JobList u w -> Int
lengthJobList JobNil = 0
lengthJobList (JobCons _ _ _ j) = 1 + lengthJobList j


-- -----------------------------------------------------------------------------
-- IVar

-- | A synchronisation point.  It either contains a value, or a list
-- of computations waiting for the value.
#ifdef PROFILING
data IVar u w a = IVar
  { ivarRef :: {-# UNPACK #-} !(IORef (IVarContents u w a))
  , ivarCCS :: {-# UNPACK #-} !(Ptr CostCentreStack)
#else
newtype IVar u w a = IVar
  { ivarRef :: IORef (IVarContents u w a)
#endif
  }

data IVarContents u w a
  = IVarFull (ResultVal a w)
  | IVarEmpty (JobList u w)
    -- morally this is a list of @a -> GenHaxl u w ()@, but instead of
    -- using a function, each computation begins with `getIVar` to grab
    -- the value it is waiting for.  This is less type safe but a little
    -- faster (benchmarked with tests/MonadBench.hs).

newIVar :: IO (IVar u w a)
newIVar = do
  ivarRef <- newIORef (IVarEmpty JobNil)
#ifdef PROFILING
  ivarCCS <- getCurrentCCS ivarRef
#endif
  return IVar{..}

newFullIVar :: ResultVal a w -> IO (IVar u w a)
newFullIVar r = do
  ivarRef <- newIORef (IVarFull r)
#ifdef PROFILING
  ivarCCS <- getCurrentCCS ivarRef
#endif
  return IVar{..}

withCurrentCCS :: IVar u w a -> IO (IVar u w a)
#ifdef PROFILING
withCurrentCCS ivar = do
  ccs <- getCurrentCCS ivar
  return ivar{ivarCCS = ccs}
#else
withCurrentCCS = return
#endif

getIVar :: IVar u w a -> GenHaxl u w a
getIVar i@IVar{ivarRef = !ref} = GenHaxl $ \Env{..} -> do
  e <- readIORef ref
  case e of
    IVarFull (Ok a _wt) -> return (Done a)
    IVarFull (ThrowHaxl e _wt) -> raiseFromIVar i e
    IVarFull (ThrowIO e) -> throwIO e
    IVarEmpty _ -> return (Blocked i (Return i))

-- Just a specialised version of getIVar, for efficiency in <*>
getIVarApply :: IVar u w (a -> b) -> a -> GenHaxl u w b
getIVarApply i@IVar{ivarRef = !ref} a = GenHaxl $ \Env{..} -> do
  e <- readIORef ref
  case e of
    IVarFull (Ok f _wt) -> return (Done (f a))
    IVarFull (ThrowHaxl e _wt) -> raiseFromIVar i e
    IVarFull (ThrowIO e) -> throwIO e
    IVarEmpty _ ->
      return (Blocked i (Cont (getIVarApply i a)))

-- Another specialised version of getIVar, for efficiency in cachedComputation
getIVarWithWrites :: IVar u w a -> GenHaxl u w a
getIVarWithWrites i@IVar{ivarRef = !ref} = GenHaxl $ \Env{..} -> do
  e <- readIORef ref
  case e of
    IVarFull (Ok a wt) -> do
      mbModifyWLRef wt writeLogsRef
      return (Done a)
    IVarFull (ThrowHaxl e wt) -> do
      mbModifyWLRef wt writeLogsRef
      raiseFromIVar i e
    IVarFull (ThrowIO e) -> throwIO e
    IVarEmpty _ ->
      return (Blocked i (Cont (getIVarWithWrites i)))

putIVar :: IVar u w a -> ResultVal a w -> Env u w -> IO ()
putIVar IVar{ivarRef = !ref} a Env{..} = do
  e <- readIORef ref
  case e of
    IVarEmpty jobs -> do
      writeIORef ref (IVarFull a)
      modifyIORef' runQueueRef (appendJobList jobs)
      -- An IVar is typically only meant to be written to once
      -- so it would make sense to throw an error here. But there
      -- are legitimate use-cases for writing several times.
      -- (See Haxl.Core.Parallel)
    IVarFull{} -> return ()

{-# INLINE addJob #-}
addJob :: Env u w -> GenHaxl u w b -> IVar u w b -> IVar u w a -> IO ()
addJob env !haxl !resultIVar IVar{ivarRef = !ref} =
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
data ResultVal a w
  = Ok a (WriteTree w)
  | ThrowHaxl SomeException (WriteTree w)
  | ThrowIO SomeException
    -- we get no write logs when an IO exception occurs

done :: ResultVal a w -> IO (Result u w a)
done (Ok a _) = return (Done a)
done (ThrowHaxl e _) = raise e
done (ThrowIO e) = throwIO e

eitherToResultThrowIO :: Either SomeException a -> ResultVal a w
eitherToResultThrowIO (Right a) = Ok a NilWrites
eitherToResultThrowIO (Left e)
  | Just HaxlException{} <- fromException e = ThrowHaxl e NilWrites
  | otherwise = ThrowIO e

eitherToResult :: Either SomeException a -> ResultVal a w
eitherToResult (Right a) = Ok a NilWrites
eitherToResult (Left e) = ThrowHaxl e NilWrites


-- -----------------------------------------------------------------------------
-- CompleteReq

-- | A completed request from a data source, containing the result,
-- and the 'IVar' representing the blocked computations.  The job of a
-- data source is just to add these to a queue ('completions') using
-- 'putResult'; the scheduler collects them from the queue and unblocks
-- the relevant computations.
data CompleteReq u w
  = forall a . CompleteReq
      (Either SomeException a)
      !(IVar u w a)  -- IVar because the result is cached
      {-# UNPACK #-} !Int64 -- see Note [tracking allocation in child threads]


{- Note [tracking allocation in child threads]

For a BackgroundFetch, we might be doing some of the work in a
separate thread, but we want to make sure that the parent thread gets
charged for the allocation, so that allocation limits still work.

The design is a bit tricky here.  We want to track the allocation
accurately but without adding much overhead.

The best way to propagate the allocation back from the child thread is
through putResult.  If we had some other method, we would also need a
way to synchronise it with the main runHaxl loop; the advantage of
putResult is that this is already a synchronisation method, because
runHaxl is waiting for the result of the dataFetch.

(slight wrinkle here: runHaxl might not wait for the result of the
dataFetch in the case where we do some speculative execution in
pAnd/pOr)

We need a special version of putResult for child threads
(putResultFromChildThread), because we don't want to propagate any
allocation from the runHaxl thread back to itself and count it twice.

We also want to capture the allocation as late as possible, so that we
count everything.  For that reason, we pass a Bool down from putResult
into the function in the ResultVar, and it reads the allocation
counter as the last thing before adding the result to the completions
TVar.

The other problem to consider is how to capture the allocation when
the child thread is doing multiple putResults.  Our solution here is
to ensure that the *last* one is a putResultFromChildThread, so it
captures all the allocation from everything leading up to it.

Why not reset the counter each time, so we could do multiple
putResultFromChildThreads?  Because the child thread might be using an
allocation limit itself, and changing the counter would mess it up.
-}

-- -----------------------------------------------------------------------------
-- Result

-- | The result of a computation is either 'Done' with a value, 'Throw'
-- with an exception, or 'Blocked' on the result of a data fetch with
-- a continuation.
data Result u w a
  = Done a
  | Throw SomeException
  | forall b . Blocked
      {-# UNPACK #-} !(IVar u w b)
      (Cont u w a)
         -- ^ The 'IVar' is what we are blocked on; 'Cont' is the
         -- continuation.  This might be wrapped further if we're
         -- nested inside multiple '>>=', before finally being added
         -- to the 'IVar'.  Morally @b -> GenHaxl u w a@, but see
         -- 'IVar',

instance (Show a) => Show (Result u w a) where
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
data Cont u w a
  = Cont (GenHaxl u w a)
  | forall b. Cont u w b :>>= (b -> GenHaxl u w a)
  | forall b. (b -> a) :<$> (Cont u w b)
  | Return (IVar u w a)

toHaxl :: Cont u w a -> GenHaxl u w a
toHaxl (Cont haxl) = haxl
toHaxl (m :>>= k) = toHaxlBind m k
toHaxl (f :<$> x) = toHaxlFmap f x
toHaxl (Return i) = getIVar i

toHaxlBind :: Cont u w b -> (b -> GenHaxl u w a) -> GenHaxl u w a
toHaxlBind (m :>>= k) k2 = toHaxlBind m (k >=> k2)
toHaxlBind (Cont haxl) k = haxl >>= k
toHaxlBind (f :<$> x) k = toHaxlBind x (k . f)
toHaxlBind (Return i) k = getIVar i >>= k

toHaxlFmap :: (a -> b) -> Cont u w a -> GenHaxl u w b
toHaxlFmap f (m :>>= k) = toHaxlBind m (k >=> return . f)
toHaxlFmap f (Cont haxl) = f <$> haxl
toHaxlFmap f (g :<$> x) = toHaxlFmap (f . g) x
toHaxlFmap f (Return i) = f <$> getIVar i

-- -----------------------------------------------------------------------------
-- Monad/Applicative instances

instance Monad (GenHaxl u w) where
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

instance Functor (GenHaxl u w) where
  fmap f (GenHaxl m) = GenHaxl $ \env -> do
    r <- m env
    case r of
      Done a -> return (Done (f a))
      Throw e -> return (Throw e)
      Blocked ivar cont -> trace_ "fmap Blocked" $
        return (Blocked ivar (f :<$> cont))

instance Applicative (GenHaxl u w) where
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
          Blocked ivar2 acont -> trace_ "Blocked/Blocked" $
            blockedBlocked env ivar1 fcont ivar2 acont
             -- Note [Blocked/Blocked]

blockedBlocked
  :: Env u w
  -> IVar u w c
  -> Cont u w (a -> b)
  -> IVar u w d
  -> Cont u w a
  -> IO (Result u w b)
blockedBlocked env ivar1 fcont _ acont | speculative env /= 0 =
  return (Blocked ivar1 (Cont (toHaxl fcont <*> toHaxl acont)))
blockedBlocked _ _ (Return i) ivar2 acont =
  return (Blocked ivar2 (acont :>>= getIVarApply i))
blockedBlocked _ _ (g :<$> Return i) ivar2 acont =
  return (Blocked ivar2 (acont :>>= \ a -> (\f -> g f a) <$> getIVar i))
blockedBlocked env ivar1 fcont ivar2 acont = do
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
-- Env utils

-- | Extracts data from the 'Env'.
env :: (Env u w -> a) -> GenHaxl u w a
env f = GenHaxl $ \env -> return (Done (f env))

-- | Returns a version of the Haxl computation which always uses the
-- provided 'Env', ignoring the one specified by 'runHaxl'.
withEnv :: Env u w -> GenHaxl u w a -> GenHaxl u w a
withEnv newEnv (GenHaxl m) = GenHaxl $ \_env -> do
  r <- m newEnv
  case r of
    Done a -> return (Done a)
    Throw e -> return (Throw e)
    Blocked ivar k ->
      return (Blocked ivar (Cont (withEnv newEnv (toHaxl k))))

nextCallId :: Env u w -> IO CallId
nextCallId env = atomicModifyIORef' (callIdRef env) $ \x -> (x+1,x+1)

#ifdef PROFILING
-- -----------------------------------------------------------------------------
-- CallGraph recording

-- | Returns a version of the Haxl computation which records function calls in
-- an edge list which is the function call graph. Each function that is to be
-- recorded must be wrapped with a call to @withCallGraph@.
withCallGraph
  :: Typeable a
  => (a -> Maybe Text)
  -> QualFunction
  -> GenHaxl u w a
  -> GenHaxl u w a
withCallGraph toText f a = do
  coreEnv <- env id
  -- TODO: Handle exceptions
  value <- withEnv coreEnv{currFunction = f} a
  case callGraphRef coreEnv of
    Just graph -> unsafeLiftIO $ modifyIORef' graph
      (updateCallGraph (f, currFunction coreEnv) (toText value))
    _ -> throw $ CriticalError
      "withCallGraph called without an IORef CallGraph"
  return value
  where
    updateCallGraph :: FunctionCall -> Maybe Text -> CallGraph -> CallGraph
    updateCallGraph fnCall@(childQFunc, _) (Just value) (edgeList, valueMap) =
      (fnCall : edgeList, Map.insert childQFunc value valueMap)
    updateCallGraph fnCall Nothing (edgeList, valueMap) =
      (fnCall : edgeList, valueMap)
#endif

-- -----------------------------------------------------------------------------
-- Exceptions

-- | Throw an exception in the Haxl monad
throw :: (Exception e) => e -> GenHaxl u w a
throw e = GenHaxl $ \_env -> raise e

raise :: (Exception e) => e -> IO (Result u w a)
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

raiseFromIVar :: Exception e => IVar u w a -> e -> IO (Result u w b)
#ifdef PROFILING
raiseFromIVar ivar e
  | Just (HaxlException Nothing h) <- fromException somex = do
    stk <- ccsToStrings (ivarCCS ivar)
    return (Throw (toException (HaxlException (Just stk) h)))
  | otherwise
#else
raiseFromIVar _ivar e
#endif
    = return (Throw somex)
  where
    somex = toException e

-- | Catch an exception in the Haxl monad
catch :: Exception e => GenHaxl u w a -> (e -> GenHaxl u w a) -> GenHaxl u w a
catch (GenHaxl m) h = GenHaxl $ \env -> do
   r <- m env
   case r of
     Done a    -> return (Done a)
     Throw e | Just e' <- fromException e -> unHaxl (h e') env
             | otherwise -> return (Throw e)
     Blocked ivar k -> return (Blocked ivar (Cont (catch (toHaxl k) h)))

-- | Catch exceptions that satisfy a predicate
catchIf
  :: Exception e => (e -> Bool) -> GenHaxl u w a -> (e -> GenHaxl u w a)
  -> GenHaxl u w a
catchIf cond haxl handler =
  catch haxl $ \e -> if cond e then handler e else throw e

-- | Returns @'Left' e@ if the computation throws an exception @e@, or
-- @'Right' a@ if it returns a result @a@.
try :: Exception e => GenHaxl u w a -> GenHaxl u w (Either e a)
try haxl = (Right <$> haxl) `catch` (return . Left)

-- | @since 0.3.1.0
instance Catch.MonadThrow (GenHaxl u w) where throwM = Haxl.Core.Monad.throw
-- | @since 0.3.1.0
instance Catch.MonadCatch (GenHaxl u w) where catch = Haxl.Core.Monad.catch


-- -----------------------------------------------------------------------------
-- Unsafe operations

-- | Under ordinary circumstances this is unnecessary; users of the Haxl
-- monad should generally /not/ perform arbitrary IO.
unsafeLiftIO :: IO a -> GenHaxl u w a
unsafeLiftIO m = GenHaxl $ \_env -> Done <$> m

-- | Convert exceptions in the underlying IO monad to exceptions in
-- the Haxl monad.  This is morally unsafe, because you could then
-- catch those exceptions in Haxl and observe the underlying execution
-- order.  Not to be exposed to user code.
--
-- Note: this function does not catch async exceptions. This is a flaw in Haxl
-- where it can sometimes leave the environment in a bad state when async
-- exceptions are thrown (for example the cache may think a fetch is happening
-- but the exception has stopped it). TODO would be to make Haxl async exception
-- safe and then remove the rethrowAsyncExceptions below, but for now this is
-- safer to avoid bugs. Additionally this would not protect you from async
-- exceptions thrown while executing code in the scheduler, and so relying on
-- this function to catch all async exceptions would be ambitious at best.
unsafeToHaxlException :: GenHaxl u w a -> GenHaxl u w a
unsafeToHaxlException (GenHaxl m) = GenHaxl $ \env -> do
  r <- m env `Exception.catch` \e -> do
    rethrowAsyncExceptions e
    return (Throw e)
  case r of
    Blocked cvar c ->
      return (Blocked cvar (Cont (unsafeToHaxlException (toHaxl c))))
    other -> return other

-- | Like 'try', but lifts all exceptions into the 'HaxlException'
-- hierarchy.  Uses 'unsafeToHaxlException' internally.  Typically
-- this is used at the top level of a Haxl computation, to ensure that
-- all exceptions are caught.
tryToHaxlException :: GenHaxl u w a -> GenHaxl u w (Either HaxlException a)
tryToHaxlException h = left asHaxlException <$> try (unsafeToHaxlException h)


-- -----------------------------------------------------------------------------

-- | Dump the contents of the cache as Haskell code that, when
-- compiled and run, will recreate the same cache contents.  For
-- example, the generated code looks something like this:
--
-- > loadCache :: GenHaxl u w ()
-- > loadCache = do
-- >   cacheRequest (ListWombats 3) (Right ([1,2,3]))
-- >   cacheRequest (CountAardvarks "abcabc") (Right (2))
--
dumpCacheAsHaskell :: GenHaxl u w String
dumpCacheAsHaskell =
    dumpCacheAsHaskellFn "loadCache" "GenHaxl u w ()" "cacheRequest"

-- | Dump the contents of the cache as Haskell code that, when
-- compiled and run, will recreate the same cache contents.
-- Does not take into account the writes done as part of the computation.
--
-- Takes the name and type for the resulting function as arguments.
-- Also takes the cacheFn to use, we can use either @cacheRequest@ or
-- @dupableCacheRequest@.
dumpCacheAsHaskellFn :: String -> String -> String -> GenHaxl u w String
dumpCacheAsHaskellFn fnName fnType cacheFn = do
  cache <- env dataCache  -- NB. dataCache, not memoCache.  We ignore memoized
                       -- results when dumping the cache.
  let
    readIVar (DataCacheItem IVar{ivarRef = !ref} _) = do
      r <- readIORef ref
      case r of
        IVarFull (Ok a _) -> return (Just (Right a))
        IVarFull (ThrowHaxl e _) -> return (Just (Left e))
        IVarFull (ThrowIO e) -> return (Just (Left e))
        IVarEmpty _ -> return Nothing

    mk_cr (req, res) =
      text cacheFn <+> parens (text req) <+> parens (result res)
    result (Left e) = text "except" <+> parens (text (show e))
    result (Right s) = text "Right" <+> parens (text s)

  entries <- unsafeLiftIO $ do
    showCache cache readIVar

  let
    body = if null entries
      then text "return ()"
      else vcat (map mk_cr (concatMap snd entries))

  return $ show $
    text (fnName ++ " :: " ++ fnType) $$
    text (fnName ++ " = do") $$
      nest 2 body $$
    text "" -- final newline
