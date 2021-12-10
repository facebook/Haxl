-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module ExceptionStackTests (tests) where

import Prelude ()
import Haxl.Core
import Haxl.Prelude

import Test.HUnit

import qualified ExampleDataSource

testEnv :: ReportFlags -> IO (Env () ())
testEnv report = do
  exstate <- ExampleDataSource.initGlobalState
  let st = stateSet exstate stateEmpty
  env <- initEnv st ()
  return env{ flags = (flags env){ report = report } }

reportFlags :: ReportFlags
reportFlags = setReportFlag ReportExceptionLabelStack defaultReportFlags

runHaxlTest
  :: ReportFlags
  -> String
  -> (Int -> Int -> GenHaxl () () Int)
  -> IO (Maybe [Text])
runHaxlTest report str func = do
  env <- testEnv report
  result <- runHaxl env $
    withLabel "try" $ tryToHaxlException $ withLabel "test" $ do
      x <- withLabel "dummy" $ pure 1
      y <- withLabel "fetch" $ ExampleDataSource.countAardvarks str
      withLabel "func" $ func x y
  case result of
    Left (HaxlException stk _) -> return stk
    Right{} -> assertFailure "expected: HaxlException"

fetchException :: Test
fetchException = TestCase $ do
  result <- runHaxlTest reportFlags "BANG4" $ \i j -> return $ i + j
  assertEqual "stack" (Just ["fetch", "test", "try", "MAIN"]) result

userException :: Test
userException = TestCase $ do
  result <- runHaxlTest reportFlags "aaa" $ \_ _ -> withLabel "throw" $
    throw $ InvalidParameter "throw"
  assertEqual "stack" (Just ["throw", "func", "test", "try", "MAIN"]) result

#ifndef PROFILING
disabledExceptionStack :: Test
disabledExceptionStack = TestCase $ do
  result <- runHaxlTest defaultReportFlags "BANG4" $ \i j -> return $ i + j
  assertEqual "stack" Nothing result
#endif

tests :: Test
tests = TestList
  [ TestLabel "FetchException" fetchException
  , TestLabel "UserException" userException
#ifndef PROFILING
  , TestLabel "DisabledExceptionStack"  disabledExceptionStack
#endif
  ]
