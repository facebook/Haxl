-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module ProfileTests where

import Haxl.Prelude

import Haxl.Core
import Haxl.Core.Monad
import Haxl.Core.Stats
import Haxl.DataSource.ConcurrentIO

import Test.HUnit

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Aeson
import Data.IORef
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet

import TestUtils
import WorkDataSource

mkProfilingEnv = do
  env <- makeTestEnv False
  return env { flags = (flags env) { report = 4 } }

collectsdata :: Assertion
collectsdata = do
  e <- mkProfilingEnv
  _x <- runHaxl e $
          withLabel "bar" $
            withLabel "foo" $ do
              u <- env userEnv
              -- do some non-trivial work that can't be lifted out
              case fromJSON <$> HashMap.lookup "A" u of
                Just (Success n) | sum [n .. 1000::Integer] > 0 -> return 5
                _otherwise -> return (4::Int)
  profData <- profile <$> readIORef (profRef e)
  assertEqual "has data" 3 $ HashMap.size profData
  assertBool "foo allocates" $
    case profileAllocs <$> HashMap.lookup "foo" profData of
      Just x -> x > 10000
      Nothing -> False
  assertBool "bar does not allocate (much)" $
    case profileAllocs <$> HashMap.lookup "bar" profData of
      Just n -> n < 5000  -- getAllocationCounter can be off by +/- 4K
      _otherwise -> False
  assertEqual "foo's parent" (Just ["bar"]) $
    HashSet.toList . profileDeps <$> HashMap.lookup "foo" profData

exceptions :: Assertion
exceptions = do
  env <- mkProfilingEnv
  _x <- runHaxl env $
          withLabel "outer" $
            tryToHaxlException $ withLabel "inner" $
              unsafeLiftIO $ evaluate $ force (error "pure exception" :: Int)
  profData <- profile <$> readIORef (profRef env)
  assertBool "inner label not added" $
    not $ HashMap.member "inner" profData

  env2 <- mkProfilingEnv
  _x <- runHaxl env2 $
          withLabel "outer" $
            tryToHaxlException $ withLabel "inner" $
              throw $ NotFound "haxl exception"
  profData <- profile <$> readIORef (profRef env2)
  assertBool "inner label added" $
    HashMap.member "inner" profData


-- Test that we correctly attribute work done in child threads when
-- using BackgroundFetch to the caller of runHaxl. This is important
-- for correct accounting when relying on allocation limits.
threadAlloc :: Assertion
threadAlloc = do
  st <- mkConcurrentIOState
  env <- initEnv (stateSet st stateEmpty) ()
  a0 <- getAllocationCounter
  _x <- runHaxl env $ work 100000
  a1 <- getAllocationCounter
  assertBool "threadAlloc" $ (a0 - a1) > 1000000
    -- the result was 16MB on 64-bit, or around 25KB if we miss the allocs
    -- in the child thread.


tests = TestList
  [ TestLabel "collectsdata" $ TestCase collectsdata
  , TestLabel "exceptions" $ TestCase exceptions
  , TestLabel "threads" $ TestCase threadAlloc
  ]
