-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file. An additional grant of patent rights can
-- be found in the PATENTS file.

{-# LANGUAGE CPP #-}

-- |
-- The 'Flags' type and related functions.  This module is provided
-- for access to Haxl internals only; most users should import
-- "Haxl.Core" instead.
--
module Haxl.Core.Flags
  (
    -- * Tracing flags
    Flags(..)
  , defaultFlags
  , ifTrace
  , ifReport
  , ifProfiling
  ) where

import Control.Monad

-- ---------------------------------------------------------------------------
-- Flags

-- | Flags that control the operation of the engine.
data Flags = Flags
  { trace :: {-# UNPACK #-} !Int
    -- ^ Tracing level (0 = quiet, 3 = very verbose).
  , report :: {-# UNPACK #-} !Int
    -- ^ Report level:
    --    * 0 = quiet
    --    * 1 = quiet (legacy, this used to do something)
    --    * 2 = data fetch stats & errors
    --    * 3 = (same as 2, this used to enable errors)
    --    * 4 = profiling
    --    * 5 = log stack traces of dataFetch calls
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
  , report = 0
  , caching = 1
  , recording = 0
  }

#if __GLASGOW_HASKELL__ >= 710
#define FUNMONAD Monad m
#else
#define FUNMONAD (Functor m, Monad m)
#endif

-- | Runs an action if the tracing level is above the given threshold.
ifTrace :: FUNMONAD => Flags -> Int -> m a -> m ()
ifTrace flags i = when (trace flags >= i) . void

-- | Runs an action if the report level is above the given threshold.
ifReport :: FUNMONAD => Flags -> Int -> m a -> m ()
ifReport flags i = when (report flags >= i) . void

ifProfiling :: FUNMONAD => Flags -> m a -> m ()
ifProfiling flags = when (report flags >= 4) . void

#undef FUNMONAD
