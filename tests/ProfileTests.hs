{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module ProfileTests where

import Haxl.Prelude
import Data.List

import Haxl.Core
import Haxl.Core.Monad

import Test.HUnit

import Control.DeepSeq (force)
import Control.Exception (evaluate)
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
  profData <- profile <$> readIORef (profRef env)
  assertEqual "has data" 3 $ HashMap.size profData
  assertBool "foo allocates" $
    case profileAllocs <$> HashMap.lookup "foo" profData of
      Just x -> x > 0
      Nothing -> False
  assertEqual "bar does not allocate" (Just 0) $
    profileAllocs <$> HashMap.lookup "bar" profData
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

tests = TestList
  [ TestLabel "collectsdata" $ TestCase collectsdata
  , TestLabel "exceptions" $ TestCase exceptions
  ]
