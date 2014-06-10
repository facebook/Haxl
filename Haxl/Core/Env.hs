-- Copyright (c) 2014, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file. An additional grant of patent rights can
-- be found in the PATENTS file.

{-# LANGUAGE OverloadedStrings #-}

-- | The Haxl monad environment.
module Haxl.Core.Env
  ( Env(..)
  , emptyEnv
  , initEnv
  , Caches
  , initEnvWithData
  , caches
  ) where

import Haxl.Core.DataCache as DataCache
import Haxl.Core.StateStore
import Haxl.Core.Types

import Data.IORef

-- | The data we carry around in the Haxl monad.
data Env u = Env
  { cacheRef     :: IORef DataCache     -- cached data fetches
  , memoRef      :: IORef DataCache     -- memoized computations
  , flags        :: Flags
  , userEnv      :: u
  , statsRef     :: IORef Stats
  , states       :: StateStore
  -- ^ Data sources and other components can store their state in
  -- here. Items in this store must be instances of 'StateKey'.
  }

type Caches = (IORef DataCache, IORef DataCache)

caches :: Env u -> Caches
caches env = (cacheRef env, memoRef env)

-- | Initialize an environment with a 'StateStore', an input map, a
-- preexisting 'DataCache', and a seed for the random number generator.
initEnvWithData :: StateStore -> u -> Caches -> IO (Env u)
initEnvWithData states e (cref, mref) = do
  sref <- newIORef emptyStats
  return Env
    { cacheRef = cref
    , memoRef = mref
    , flags = defaultFlags
    , userEnv = e
    , states = states
    , statsRef = sref
    }

-- | Initializes an environment with 'DataStates' and an input map.
initEnv :: StateStore -> u -> IO (Env u)
initEnv states e = do
  cref <- newIORef DataCache.empty
  mref <- newIORef DataCache.empty
  initEnvWithData states e (cref,mref)

-- | A new, empty environment.
emptyEnv :: u -> IO (Env u)
emptyEnv = initEnv stateEmpty
