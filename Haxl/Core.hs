{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

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
  , initEnvWithData, initEnv, emptyEnv, sanitizeEnv
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
  , pAnd, pOr, unsafeChooseFirst

    -- ** Statistics
  , Stats(..)
  , FetchStats(..)
  , CallId
  , Microseconds
  , Timestamp
  , emptyStats
  , numFetches
  , ppStats
  , ppFetchStats
  , aggregateFetchBatches
  , Profile(..)
  , ProfileMemo(..)
  , ProfileFetch(..)
  , emptyProfile
  , ProfileLabel
  , ProfileKey
  , ProfileData(..)
  , emptyProfileData
  , AllocCount
  , LabelHitCount

    -- * Report flags
  , ReportFlag(..)
  , ReportFlags
  , defaultReportFlags
  , profilingReportFlags
  , setReportFlag
  , clearReportFlag
  , testReportFlag

    -- ** Flags
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
  , FailureClassification(..)

    -- ** Result variables
  , ResultVar(..)
  , mkResultVar
  , putFailure
  , putResult
  , putSuccess
  , putResultFromChildThread
  , putResultWithStats
  , putResultWithStatsFromChildThread
  , DataSourceStats(..)

    -- ** Default fetch implementations
  , asyncFetch, asyncFetchWithDispatch, asyncFetchAcquireRelease
  , backgroundFetchSeq, backgroundFetchPar
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
