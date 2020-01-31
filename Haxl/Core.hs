-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

-- | Everything needed to define data sources and to invoke the
-- engine.
--
{-# LANGUAGE CPP #-}
module Haxl.Core (
    -- * The monad and operations
    GenHaxl (..), runHaxl, runHaxlWithWrites

    -- ** Env
  , Env(..), Caches, caches
    -- *** Operations in the monad
  , env, withEnv, withLabel
    -- *** Building the Env
  , initEnvWithData, initEnv, emptyEnv
    -- *** Building the StateStore
  , StateStore, stateGet, stateSet, stateEmpty

    -- ** Writes inside the monad
  , tellWrite, tellWriteNoMemo

    -- ** Exceptions
  , throw, catch, catchIf, try, tryToHaxlException

    -- ** Data fetching and caching
  , dataFetch, uncachedRequest
  , cacheRequest, dupableCacheRequest, cacheResult, cacheResultWithShow
  , cachedComputation, preCacheComputation
  , dumpCacheAsHaskell

    -- ** Memoization
  , newMemo, newMemoWith, prepareMemo, runMemo
  , memo, memoUnique, memoize, memoize1, memoize2
  , memoFingerprint, MemoFingerprintKey(..)

    -- ** Conditionals
  , pAnd, pOr

    -- ** Statistics
  , Stats(..)
  , FetchStats(..)
  , Microseconds
  , Timestamp
  , emptyStats
  , numFetches
  , ppStats
  , ppFetchStats
  , aggregateFetchBatches
  , Profile
  , emptyProfile
  , profile
  , ProfileLabel
  , ProfileData(..)
  , emptyProfileData
  , AllocCount
  , LabelHitCount
  , MemoHitCount

    -- ** Tracing flags
  , Flags(..)
  , defaultFlags
  , ifTrace
  , ifReport
  , ifProfiling

    -- * Building data sources
  , DataSource(..)
  , ShowP(..)
  , DataSourceName(..)
  , Request
  , BlockedFetch(..)
  , PerformFetch(..)
  , StateKey(..)
  , SchedulerHint(..)

    -- ** Result variables
  , ResultVar(..)
  , mkResultVar
  , putFailure
  , putResult
  , putSuccess
  , putResultFromChildThread

    -- ** Default fetch implementations
  , asyncFetch, asyncFetchWithDispatch, asyncFetchAcquireRelease
  , backgroundFetchAcquireRelease, backgroundFetchAcquireReleaseMVar
  , stubFetch
  , syncFetch

    -- ** Utilities
  , except
  , setError
  , getMapFromRCMap

    -- * Exceptions
  , module Haxl.Core.Exception

    -- * Recording the function callgraph
  , module Haxl.Core.CallGraph
  ) where

import Haxl.Core.CallGraph
import Haxl.Core.DataSource
import Haxl.Core.Flags
import Haxl.Core.Memo
import Haxl.Core.Monad hiding (unsafeLiftIO {- Ask nicely to get this! -})
import Haxl.Core.Fetch
import Haxl.Core.Parallel
import Haxl.Core.Profile
import Haxl.Core.Run
import Haxl.Core.Stats
import Haxl.Core.Exception
import Haxl.Core.RequestStore (getMapFromRCMap)
import Haxl.Core.ShowP (ShowP(..))
import Haxl.Core.StateStore
