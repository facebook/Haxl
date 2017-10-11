-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file. An additional grant of patent rights can
-- be found in the PATENTS file.

-- | Everything needed to define data sources and to invoke the
-- engine.
--
module Haxl.Core (
  -- * The monad and operations
  GenHaxl (..), runHaxl,

  -- ** Env
  Env(..), Caches, caches,
  -- *** Operations in the monad
  env, withEnv, withLabel,
  -- *** Building the Env
  initEnvWithData, initEnv, emptyEnv,
  -- *** Building the StateStore
  StateStore, stateGet, stateSet, stateEmpty,

  -- ** Exceptions
  throw, catch, catchIf, try, tryToHaxlException,

  -- ** Data fetching and caching
  dataFetch, uncachedRequest,
  cacheRequest, cacheResult, cacheResultWithShow,
  cachedComputation, preCacheComputation,
  dumpCacheAsHaskell,

  -- ** Memoization
  newMemo, newMemoWith, prepareMemo, runMemo,
  memo, memoize, memoize1, memoize2,
  memoFingerprint, MemoFingerprintKey(..),

  -- ** Conditionals
  pAnd, pOr,

  -- ** Statistics
  Stats(..),
  FetchStats(..),
  Microseconds,
  emptyStats,
  numRounds,
  numFetches,
  ppStats,
  ppFetchStats,
  Profile,
  emptyProfile,
  profile,
  ProfileLabel,
  ProfileData(..),
  emptyProfileData,

  -- ** Tracing flags
  Flags(..),
  defaultFlags,
  ifTrace,
  ifReport,
  ifProfiling,

  -- * Building data sources
  DataSource(..),
  ShowP(..),
  DataSourceName(..),
  Request,
  BlockedFetch(..),
  PerformFetch(..),
  StateKey(..),
  SchedulerHint(..),

  -- ** Result variables
  ResultVar(..),
  mkResultVar,
  putFailure,
  putResult,
  putSuccess,

  -- ** Default fetch implementations
  asyncFetch, asyncFetchWithDispatch, asyncFetchAcquireRelease,
  stubFetch,
  syncFetch,

  -- ** Utilities
  except,
  setError,

  -- * Exceptions
  module Haxl.Core.Exception
  ) where

import Haxl.Core.DataSource
import Haxl.Core.Flags
import Haxl.Core.Memo
import Haxl.Core.Monad hiding (unsafeLiftIO {- Ask nicely to get this! -})
import Haxl.Core.Monad.Fetch
import Haxl.Core.Monad.Parallel
import Haxl.Core.Monad.Profile
import Haxl.Core.Monad.Schedule
import Haxl.Core.Stats
import Haxl.Core.Exception
import Haxl.Core.ShowP (ShowP(..))
import Haxl.Core.StateStore
