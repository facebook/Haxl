-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- The 'DataSource' class and related types and functions.  This
-- module is provided for access to Haxl internals only; most users
-- should import "Haxl.Core" instead.
--
module Haxl.Core.DataSource
  (
  -- * Data fetching
    DataSource(..)
  , DataSourceName(..)
  , Request
  , BlockedFetch(..)
  , PerformFetch(..)
  , SchedulerHint(..)
  , FailureClassification(..)

  -- * Result variables
  , ResultVar(..)
  , mkResultVar
  , putFailure
  , putResult
  , putResultFromChildThread
  , putSuccess

  -- * Default fetch implementations
  , asyncFetch, asyncFetchWithDispatch
  , asyncFetchAcquireRelease
  , backgroundFetchSeq, backgroundFetchPar
  , backgroundFetchAcquireRelease
  , backgroundFetchAcquireReleaseMVar
  , stubFetch
  , syncFetch

  -- * Utilities
  , except
  , setError
  ) where

import Control.Exception
import Control.Monad
import Data.Hashable
import Data.Text (Text)
import Data.Typeable

import Haxl.Core.Exception
import Haxl.Core.Flags
import Haxl.Core.ShowP
import Haxl.Core.StateStore


import GHC.Conc ( newStablePtrPrimMVar
                , PrimMVar)
import Control.Concurrent ( threadCapability
                          , forkOn
                          , myThreadId )
import Control.Concurrent.MVar
import Foreign.StablePtr
import System.Mem (setAllocationCounter)

-- ---------------------------------------------------------------------------
-- DataSource class

-- | The class of data sources, parameterised over the request type for
-- that data source. Every data source must implement this class.
--
-- A data source keeps track of its state by creating an instance of
-- 'StateKey' to map the request type to its state. In this case, the
-- type of the state should probably be a reference type of some kind,
-- such as 'IORef'.
--
-- For a complete example data source, see
-- <https://github.com/facebook/Haxl/tree/master/example Examples>.
--
class (DataSourceName req, StateKey req, ShowP req) => DataSource u req where

  -- | Issues a list of fetches to this 'DataSource'. The 'BlockedFetch'
  -- objects contain both the request and the 'ResultVar's into which to put
  -- the results.
  fetch
    :: State req
      -- ^ Current state.
    -> Flags
      -- ^ Tracing flags.
    -> u
      -- ^ User environment.
    -> PerformFetch req
      -- ^ Fetch the data; see 'PerformFetch'.

  schedulerHint :: u -> SchedulerHint req
  schedulerHint _ = TryToBatch

  classifyFailure :: u -> req a -> SomeException -> FailureClassification
  classifyFailure _ _ _ = StandardFailure

class DataSourceName (req :: * -> *) where
  -- | The name of this 'DataSource', used in tracing and stats. Must
  -- take a dummy request.
  dataSourceName :: Proxy req -> Text

-- The 'ShowP' class is a workaround for the fact that we can't write
-- @'Show' (req a)@ as a superclass of 'DataSource', without also
-- parameterizing 'DataSource' over @a@, which is a pain (I tried
-- it). 'ShowP' seems fairly benign, though.

-- | A convenience only: package up 'Eq', 'Hashable', 'Typeable', and 'Show'
-- for requests into a single constraint.
type Request req a =
  ( Eq (req a)
  , Hashable (req a)
  , Typeable (req a)
  , Show (req a)
  , Show a
  )

-- | Hints to the scheduler about this data source
data SchedulerHint (req :: * -> *)
  = TryToBatch
    -- ^ Hold data-source requests while we execute as much as we can, so
    -- that we can hopefully collect more requests to batch.
  | SubmitImmediately
    -- ^ Submit a request via fetch as soon as we have one, don't try to
    -- batch multiple requests.  This is really only useful if the data source
    -- returns BackgroundFetch, otherwise requests to this data source will
    -- be performed synchronously, one at a time.

-- | Hints to the stats module about how to deal with these failures
data FailureClassification
  = StandardFailure
  | IgnoredForStatsFailure

-- | A data source can fetch data in one of four ways.
--
data PerformFetch req
  = SyncFetch  ([BlockedFetch req] -> IO ())
    -- ^ Fully synchronous, returns only when all the data is fetched.
    -- See 'syncFetch' for an example.
  | AsyncFetch ([BlockedFetch req] -> IO () -> IO ())
    -- ^ Asynchronous; performs an arbitrary IO action while the data
    -- is being fetched, but only returns when all the data is
    -- fetched.  See 'asyncFetch' for an example.
  | BackgroundFetch ([BlockedFetch req] -> IO ())
    -- ^ Fetches the data in the background, calling 'putResult' at
    -- any time in the future.  This is the best kind of fetch,
    -- because it provides the most concurrency.


-- | A 'BlockedFetch' is a pair of
--
--   * The request to fetch (with result type @a@)
--
--   * A 'ResultVar' to store either the result or an error
--
-- We often want to collect together multiple requests, but they return
-- different types, and the type system wouldn't let us put them
-- together in a list because all the elements of the list must have the
-- same type. So we wrap up these types inside the 'BlockedFetch' type,
-- so that they all look the same and we can put them in a list.
--
-- When we unpack the 'BlockedFetch' and get the request and the 'ResultVar'
-- out, the type system knows that the result type of the request
-- matches the type parameter of the 'ResultVar', so it will let us take the
-- result of the request and store it in the 'ResultVar'.
--
data BlockedFetch r = forall a. BlockedFetch (r a) (ResultVar a)


-- -----------------------------------------------------------------------------
-- ResultVar

-- | A sink for the result of a data fetch in 'BlockedFetch'
newtype ResultVar a = ResultVar (Either SomeException a -> Bool -> IO ())
  -- The Bool here is True if result was returned by a child thread,
  -- rather than the main runHaxl thread.  see Note [tracking allocation in
  -- child threads]

mkResultVar :: (Either SomeException a -> Bool -> IO ()) -> ResultVar a
mkResultVar = ResultVar

putFailure :: (Exception e) => ResultVar a -> e -> IO ()
putFailure r = putResult r . except

putSuccess :: ResultVar a -> a -> IO ()
putSuccess r = putResult r . Right

putResult :: ResultVar a -> Either SomeException a -> IO ()
putResult (ResultVar io) res = io res False

-- | Like `putResult`, but used to get correct accounting when work is
-- being done in child threads.  This is particularly important for
-- data sources that are using 'BackgroundFetch', The allocation performed
-- in the child thread up to this point will be propagated back to the
-- thread that called 'runHaxl'.
--
-- Note: if you're doing multiple 'putResult' calls in the same thread
-- ensure that only the /last/ one is 'putResultFromChildThread'.  If you
-- make multiple 'putResultFromChildThread' calls, the allocation will be
-- counted multiple times.
--
-- If you are reusing a thread for multiple fetches, you should call
-- @System.Mem.setAllocationCounter 0@ after
-- 'putResultFromChildThread', so that allocation is not counted
-- multiple times.
putResultFromChildThread :: ResultVar a -> Either SomeException a -> IO ()
putResultFromChildThread (ResultVar io) res =  io res True
  -- see Note [tracking allocation in child threads]

-- | Function for easily setting a fetch to a particular exception
setError :: (Exception e) => (forall a. r a -> e) -> BlockedFetch r -> IO ()
setError e (BlockedFetch req m) = putFailure m (e req)

except :: (Exception e) => e -> Either SomeException a
except = Left . toException


-- -----------------------------------------------------------------------------
-- Fetch templates

stubFetch
  :: (Exception e) => (forall a. r a -> e)
  -> State r -> Flags -> u -> PerformFetch r
stubFetch e _state _flags _si = SyncFetch $ mapM_ (setError e)

-- | Common implementation templates for 'fetch' of 'DataSource'.
--
-- Example usage:
--
-- > fetch = syncFetch MyDS.withService MyDS.retrieve
-- >   $ \service request -> case request of
-- >     This x -> MyDS.fetchThis service x
-- >     That y -> MyDS.fetchThat service y
--
asyncFetchWithDispatch
  :: ((service -> IO ()) -> IO ())
  -- ^ Wrapper to perform an action in the context of a service.

  -> (service -> IO ())
  -- ^ Dispatch all the pending requests

  -> (service -> IO ())
  -- ^ Wait for the results

  -> (forall a. service -> request a -> IO (IO (Either SomeException a)))
  -- ^ Enqueue an individual request to the service.

  -> State request
  -- ^ Currently unused.

  -> Flags
  -- ^ Currently unused.

  -> u
  -- ^ Currently unused.

  -> PerformFetch request

asyncFetch, syncFetch
  :: ((service -> IO ()) -> IO ())
  -- ^ Wrapper to perform an action in the context of a service.

  -> (service -> IO ())
  -- ^ Dispatch all the pending requests and wait for the results

  -> (forall a. service -> request a -> IO (IO (Either SomeException a)))
  -- ^ Submits an individual request to the service.

  -> State request
  -- ^ Currently unused.

  -> Flags
  -- ^ Currently unused.

  -> u
  -- ^ Currently unused.

  -> PerformFetch request

asyncFetchWithDispatch
  withService dispatch wait enqueue _state _flags _si =
  AsyncFetch $ \requests inner -> withService $ \service -> do
    getResults <- mapM (submitFetch service enqueue) requests
    dispatch service
    inner
    wait service
    sequence_ getResults

asyncFetch withService wait enqueue _state _flags _si =
  AsyncFetch $ \requests inner -> withService $ \service -> do
    getResults <- mapM (submitFetch service enqueue) requests
    inner
    wait service
    sequence_ getResults

syncFetch withService dispatch enqueue _state _flags _si =
  SyncFetch $ \requests -> withService $ \service -> do
  getResults <- mapM (submitFetch service enqueue) requests
  dispatch service
  sequence_ getResults

backgroundFetchSeq, backgroundFetchPar
  :: (forall a. request a -> IO (Either SomeException a))
  -- ^ Run one request, will be run in a background thread

  -> State request
  -- ^ Currently unused.

  -> Flags
  -- ^ Currently unused.

  -> u
  -- ^ Currently unused.

  -> PerformFetch request

backgroundFetchSeq run _state _flags _si =
  BackgroundFetch $ \requests -> do
    (cap, _) <- threadCapability =<< myThreadId
    mask $ \restore -> void $ forkOn cap $ do
      let rethrow = rethrowFromBg requests
      restore (mapM_ runOne requests) `catch` rethrow
      where
        runOne (BlockedFetch request result) = do
          res <- run request
          putResultFromBg result res

backgroundFetchPar run _state _flags _si =
  BackgroundFetch $ \requests -> do
    (cap, _) <- threadCapability =<< myThreadId
    mapM_ (runOneInThread cap) requests
  where
    runOneInThread cap request = do
      mask $ \restore -> void $ forkOn cap $ do
        let rethrow = rethrowFromBg [request]
        restore (runOne request) `catch` rethrow
    runOne (BlockedFetch request result) = do
      res <- run request
      putResultFromBg result res


{- |
A version of 'asyncFetch' (actually 'asyncFetchWithDispatch') that
handles exceptions correctly.  You should use this instead of
'asyncFetch' or 'asyncFetchWithDispatch'.  The danger with
'asyncFetch' is that if an exception is thrown by @withService@, the
@inner@ action won't be executed, and we'll drop some data-fetches in
the same round.

'asyncFetchAcquireRelease' behaves like the following:

> asyncFetchAcquireRelease acquire release dispatch wait enqueue =
>   AsyncFetch $ \requests inner ->
>     bracket acquire release $ \service -> do
>       getResults <- mapM (submitFetch service enqueue) requests
>       dispatch service
>       inner
>       wait service
>       sequence_ getResults

except that @inner@ is run even if @acquire@, @enqueue@, or @dispatch@ throws,
/unless/ an async exception is received.
-}

asyncFetchAcquireRelease
  :: IO service
  -- ^ Resource acquisition for this datasource

  -> (service -> IO ())
  -- ^ Resource release

  -> (service -> IO ())
  -- ^ Dispatch all the pending requests and wait for the results

  -> (service -> IO ())
  -- ^ Wait for the results

  -> (forall a. service -> request a -> IO (IO (Either SomeException a)))
  -- ^ Submits an individual request to the service.

  -> State request
  -- ^ Currently unused.

  -> Flags
  -- ^ Currently unused.

  -> u
  -- ^ Currently unused.

  -> PerformFetch request

asyncFetchAcquireRelease
  acquire release dispatch wait enqueue _state _flags _si =
  AsyncFetch $ \requests inner -> mask $ \restore -> do
    r1 <- tryWithRethrow acquire
    case r1 of
      Left err -> do restore inner; throwIO (err :: SomeException)
      Right service -> do
        flip finally (release service) $ restore $ do
          r2 <- tryWithRethrow $ do
            getResults <- mapM (submitFetch service enqueue) requests
            dispatch service
            return getResults
          inner  --  we assume this cannot throw, ensured by performFetches
          case r2 of
            Left err -> throwIO (err :: SomeException)
            Right getResults -> do wait service; sequence_ getResults

-- | Used by 'asyncFetch' and 'syncFetch' to retrieve the results of
-- requests to a service.
submitFetch
  :: service
  -> (forall a. service -> request a -> IO (IO (Either SomeException a)))
  -> BlockedFetch request
  -> IO (IO ())
submitFetch service fetchFn (BlockedFetch request result)
  = (putResult result =<<) <$> fetchFn service request

putResultFromBg :: ResultVar a -> Either SomeException a -> IO ()
putResultFromBg result r = do
  -- See comment on putResultFromChildThread
  -- We must set the allocation counter to 0 here in case there are more
  -- results in the batch.
  -- This is safe as we own this thread, and know that there
  -- is no allocation limits set.
  putResultFromChildThread result r
  setAllocationCounter 0

rethrowFromBg :: [BlockedFetch req] -> SomeException -> IO ()
rethrowFromBg requests e = do
  mapM_ (rethrow1bg e) requests
  rethrowAsyncExceptions e
  where
    rethrow1bg e (BlockedFetch _ result) =
      putResultFromBg result (Left e)


backgroundFetchAcquireReleaseMVar
  :: IO service
  -- ^ Resource acquisition for this datasource

  -> (service -> IO ())
  -- ^ Resource release

  -> (service -> Int -> MVar () -> IO ())
  -- ^ Dispatch all the pending requests and when ready trigger the given mvar

  -> (service -> IO ())
  -- ^ Process all requests

  -> (forall a. service -> request a -> IO (IO (Either SomeException a)))
  -- ^ Submits an individual request to the service.

  -> State request
  -- ^ Currently unused.

  -> Flags
  -- ^ Currently unused.

  -> u
  -- ^ Currently unused.

  -> PerformFetch request

backgroundFetchAcquireReleaseMVar
  acquire release dispatch process enqueue _state _flags _si =
  BackgroundFetch $ \requests -> do
    mvar <- newEmptyMVar
    mask $ \restore -> do
      (cap, _) <- threadCapability =<< myThreadId
      service <- acquire
      getResults <- (do
        results <- restore $ mapM (submit service) requests
        -- dispatch takes ownership of mvar, so we call it under `mask` to
        -- ensure that it can safely manage that resource.
        dispatch service cap mvar
        return (sequence_ results)) `onException` release service
      -- now spawn off a background thread to wait on the dispatch to finish
      _tid <- forkOn cap $ do
        takeMVar mvar
          -- todo: it is possible that we would want to do
          -- this processResults on the main scheduler thread for performance
          -- which might reduce thread switching, especially for large batches
          -- but for now this seems to work just fine
        let rethrow = rethrowFromBg requests
        _ <- finally
          (restore (process service >> getResults) `catch` rethrow)
          (release service `catch` rethrow)
        return ()
      return ()
  where
    submit service (BlockedFetch request result) =
      (putResultFromBg result =<<) <$> enqueue service request


{- |
A version of 'backgroundFetchAcquireReleaseMVar' where the dispatch function
is given a 'StablePtr PrimMVar' which is more useful for C based APIs.
-}

backgroundFetchAcquireRelease
  :: IO service
  -- ^ Resource acquisition for this datasource

  -> (service -> IO ())
  -- ^ Resource release

  -> (service -> Int -> StablePtr PrimMVar -> IO ())
  -- ^ Dispatch all the pending requests and when ready trigger the given mvar

  -> (service -> IO ())
  -- ^ Process all requests

  -> (forall a. service -> request a -> IO (IO (Either SomeException a)))
  -- ^ Submits an individual request to the service.

  -> State request
  -- ^ Currently unused.

  -> Flags
  -- ^ Currently unused.

  -> u
  -- ^ Currently unused.

  -> PerformFetch request

backgroundFetchAcquireRelease
  a r dispatch = backgroundFetchAcquireReleaseMVar
                 a
                 r
                 (\s c mvar -> do
                     sp <- newStablePtrPrimMVar mvar
                     dispatch s c sp)
