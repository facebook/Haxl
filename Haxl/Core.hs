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
  env, withEnv,
  -- *** Building the Env
  initEnvWithData, initEnv, emptyEnv,
  -- *** Building the StateStore
  StateStore, stateGet, stateSet, stateEmpty,

  -- ** Exceptions
  throw, catch, catchIf, try, tryToHaxlException,

  -- ** Data fetching and caching
  dataFetch, uncachedRequest,
  cacheRequest, cacheResult, cachedComputation,
  dumpCacheAsHaskell,

  -- ** Memoization
  memo, memoFingerprint, MemoFingerprintKey(..),

  -- ** Statistics
  Stats(..),
  RoundStats(..),
  DataSourceRoundStats(..),
  Microseconds,
  emptyStats,
  numRounds,
  numFetches,
  ppStats,
  ppRoundStats,
  ppDataSourceRoundStats,

  -- ** Tracing flags
  Flags(..),
  defaultFlags,
  ifTrace,
  ifReport,

  -- * Building data sources
  DataSource(..),
  Show1(..),
  DataSourceName(..),
  Request,
  BlockedFetch(..),
  PerformFetch(..),
  StateKey(..),

  -- ** Result variables
  ResultVar(..),
  newEmptyResult,
  newResult,
  putFailure,
  putResult,
  putSuccess,
  takeResult,
  tryReadResult,
  tryTakeResult,

  -- ** Default fetch implementations
  asyncFetch, asyncFetchWithDispatch,
  stubFetch,
  syncFetch,

  -- ** Utilities
  except,
  setError,

  -- * Exceptions
  module Haxl.Core.Exception
  ) where

import Haxl.Core.Memo
import Haxl.Core.Monad hiding (unsafeLiftIO {- Ask nicely to get this! -})
import Haxl.Core.Types
import Haxl.Core.Exception
import Haxl.Core.Show1 (Show1(..))
import Haxl.Core.StateStore
