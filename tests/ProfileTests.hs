{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module ProfileTests where

import Haxl.Prelude
import Data.List

import Haxl.Core

import Test.HUnit

import Data.IORef
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet

import TestUtils

mkProfilingEnv = do
  env <- makeTestEnv
  return env { flags = (flags env) { report = 4 } }

collectsdata :: Assertion
collectsdata = do
  env <- mkProfilingEnv
  _x <- runHaxl env $
          withLabel "bar" $
            withLabel "foo" $
              if length (intersect ["a"::Text, "b"] ["c"]) > 1
              then return 5
              else return (4::Int)
  profData <- readIORef (profRef env)
  assertEqual "has data" 3 $ HashMap.size profData
  assertEqual "foo allocates" (Just 4152) $
    profileAllocs <$> HashMap.lookup "foo" profData
  assertEqual "bar does not allocate" (Just 0) $
    profileAllocs <$> HashMap.lookup "bar" profData
  assertEqual "foo's parent" (Just ["bar"]) $
    HashSet.toList . profileDeps <$> HashMap.lookup "foo" profData

tests = TestList
  [ TestLabel "collectsdata" $ TestCase collectsdata
  ]
