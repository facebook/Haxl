-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file. An additional grant of patent rights can
-- be found in the PATENTS file.

{-# LANGUAGE OverloadedStrings #-}
module TestUtils
  ( makeTestEnv
  , expectRoundsWithEnv
  , expectRounds
  , expectFetches
  , testinput
  , id1, id2, id3, id4
  ) where

import TestTypes
import MockTAO

import Data.IORef
import Data.Aeson
import Test.HUnit
import qualified Data.HashMap.Strict as HashMap

import Haxl.Core

import Prelude()
import Haxl.Prelude

testinput :: Object
testinput = HashMap.fromList [
  "A" .= (1 :: Int),
  "B" .= (2 :: Int),
  "C" .= (3 :: Int),
  "D" .= (4 :: Int) ]

id1 :: Haxl Id
id1 = lookupInput "A"

id2 :: Haxl Id
id2 = lookupInput "B"

id3 :: Haxl Id
id3 = lookupInput "C"

id4 :: Haxl Id
id4 = lookupInput "D"

makeTestEnv :: Bool -> IO (Env UserEnv)
makeTestEnv future = do
  tao <- MockTAO.initGlobalState future
  let st = stateSet tao stateEmpty
  env <- initEnv st testinput
  return env { flags = (flags env) { report = 2 } }

expectRoundsWithEnv
  :: (Eq a, Show a) => Int -> a -> Haxl a -> Env UserEnv -> Assertion
expectRoundsWithEnv n result haxl env = do
  a <- runHaxl env haxl
  assertEqual "result" result a
  stats <- readIORef (statsRef env)
  assertEqual "rounds" n (numRounds stats)

expectRounds :: (Eq a, Show a) => Int -> a -> Haxl a -> Bool -> Assertion
expectRounds n result haxl future = do
  env <- makeTestEnv future
  expectRoundsWithEnv n result haxl env

expectFetches :: (Eq a, Show a) => Int -> Haxl a -> Bool -> Assertion
expectFetches n haxl future = do
  env <- makeTestEnv future
  _ <- runHaxl env haxl
  stats <- readIORef (statsRef env)
  assertEqual "fetches" n (numFetches stats)
