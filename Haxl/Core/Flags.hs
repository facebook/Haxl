-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE BangPatterns #-}

-- |
-- The 'Flags' type and related functions.  This module is provided
-- for access to Haxl internals only; most users should import
-- "Haxl.Core" instead.
--
module Haxl.Core.Flags
  (
    -- * Report flags
    ReportFlag(..)
  , ReportFlags
  , defaultReportFlags
  , profilingReportFlags
  , setReportFlag
  , clearReportFlag
  , testReportFlag
    -- * Flags
  , Flags(..)
  , defaultFlags
  , ifTrace
  , ifReport
  , ifProfiling
  ) where

import Control.Monad
import Data.Bits
import Data.List (foldl')

-- ---------------------------------------------------------------------------
-- ReportFlags
data ReportFlag
  = ReportOutgoneFetches  -- ^ outgone fetches, for debugging eg: timeouts
  | ReportFetchStats  -- ^ data fetch stats & errors
  | ReportProfiling   -- ^ enabling label stack and profiling
  | ReportExceptionLabelStack  -- ^ include label stack in HaxlException
  | ReportFetchStack  -- ^ log cost-center stack traces of dataFetch calls
  deriving (Bounded, Enum, Eq, Show)

profilingDependents :: [ReportFlag]
profilingDependents =
  [ ReportExceptionLabelStack
  , ReportFetchStack
  ]

newtype ReportFlags = ReportFlags Int

defaultReportFlags :: ReportFlags
defaultReportFlags = ReportFlags 0

profilingReportFlags :: ReportFlags
profilingReportFlags = foldl' (flip setReportFlag) defaultReportFlags
  [ ReportOutgoneFetches
  , ReportFetchStats
  , ReportProfiling
  ]

setReportFlag :: ReportFlag -> ReportFlags -> ReportFlags
setReportFlag f (ReportFlags fs) =
  ReportFlags $ setDependencies $ setBit fs $ fromEnum f
  where
    setDependencies
      | f `elem` profilingDependents = flip setBit $ fromEnum ReportProfiling
      | otherwise = id

clearReportFlag :: ReportFlag -> ReportFlags -> ReportFlags
clearReportFlag f (ReportFlags fs) =
  ReportFlags $ clearDependents $ clearBit fs $ fromEnum f
  where
    clearDependents z = case f of
      ReportProfiling -> foldl' clearBit z $ map fromEnum profilingDependents
      _ -> z

{-# INLINE testReportFlag #-}
testReportFlag :: ReportFlag -> ReportFlags -> Bool
testReportFlag !f (ReportFlags !fs) = testBit fs $ fromEnum f

-- ---------------------------------------------------------------------------
-- Flags

-- | Flags that control the operation of the engine.
data Flags = Flags
  { trace :: {-# UNPACK #-} !Int
    -- ^ Tracing level (0 = quiet, 3 = very verbose).
  , report :: {-# UNPACK #-} !ReportFlags
    -- ^ Report flags
  , caching :: {-# UNPACK #-} !Int
    -- ^ Non-zero if caching is enabled.  If caching is disabled, then
    -- we still do batching and de-duplication, but do not cache
    -- results.
  , recording :: {-# UNPACK #-} !Int
    -- ^ Non-zero if recording is enabled. This allows tests to record cache
    -- calls for datasources by making uncachedRequest behave like dataFetch
  }

defaultFlags :: Flags
defaultFlags = Flags
  { trace = 0
  , report = defaultReportFlags
  , caching = 1
  , recording = 0
  }

-- | Runs an action if the tracing level is above the given threshold.
ifTrace :: Monad m => Flags -> Int -> m a -> m ()
ifTrace flags i = when (trace flags >= i) . void

-- | Runs an action if the ReportFlag is set.
ifReport :: Monad m => Flags -> ReportFlag -> m a -> m ()
ifReport flags i = when (testReportFlag i $ report flags) . void

ifProfiling :: Monad m => Flags -> m a -> m ()
ifProfiling flags = ifReport flags ReportProfiling
