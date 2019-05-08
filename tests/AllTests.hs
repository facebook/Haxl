-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE CPP, OverloadedStrings #-}
module AllTests (allTests) where

import TestExampleDataSource
import BatchTests
import CoreTests
import DataCacheTest
#if __GLASGOW_HASKELL__ >= 801
import AdoTests
import OutgoneFetchesTests
#endif
#if __GLASGOW_HASKELL__ >= 710
import ProfileTests
#endif
import MemoizationTests
import TestBadDataSource
import FullyAsyncTest
import WriteTests

import Test.HUnit

allTests :: Test
allTests = TestList
  [ TestLabel "ExampleDataSource" TestExampleDataSource.tests
  , TestLabel "BatchTests-future" $ BatchTests.tests True
  , TestLabel "BatchTests-sync" $ BatchTests.tests False
  , TestLabel "CoreTests" CoreTests.tests
  , TestLabel "DataCacheTests" DataCacheTest.tests
#if __GLASGOW_HASKELL__ >= 801
  , TestLabel "AdoTests" $ AdoTests.tests False
  , TestLabel "OutgoneFetchesTest" OutgoneFetchesTests.tests
#endif
#if __GLASGOW_HASKELL__ >= 710
  , TestLabel "ProfileTests" ProfileTests.tests
#endif
  , TestLabel "MemoizationTests" MemoizationTests.tests
  , TestLabel "BadDataSourceTests" TestBadDataSource.tests
  , TestLabel "FullyAsyncTest" FullyAsyncTest.tests
  , TestLabel "WriteTest" WriteTests.tests
  ]
