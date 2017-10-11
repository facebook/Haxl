-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file. An additional grant of patent rights can
-- be found in the PATENTS file.

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Haxl.Core.Monad.Schedule
  ( runHaxl
  ) where

import Control.Concurrent.STM
import Control.Exception as Exception
import Control.Monad
import Data.IORef
import Text.Printf
import Unsafe.Coerce

import Haxl.Core.DataCache
import Haxl.Core.Exception
import Haxl.Core.Flags
import Haxl.Core.Monad
import Haxl.Core.Monad.Fetch
import Haxl.Core.Monad.Profile
import Haxl.Core.RequestStore as RequestStore


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
    --     support batching, or if batching is pessimal.
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
