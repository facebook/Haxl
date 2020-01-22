-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE OverloadedStrings #-}
module AllTests (allTests) where

import TestExampleDataSource
import BatchTests
import CoreTests
import DataCacheTest
import AdoTests
import OutgoneFetchesTests
import ProfileTests
import MemoizationTests
import TestBadDataSource
import FullyAsyncTest
import WriteTests
import ParallelTests
import StatsTests

import Test.HUnit

allTests :: Test
allTests = TestList
  [ TestLabel "ExampleDataSource" TestExampleDataSource.tests
  , TestLabel "BatchTests-future" $ BatchTests.tests True
  , TestLabel "BatchTests-sync" $ BatchTests.tests False
  , TestLabel "CoreTests" CoreTests.tests
  , TestLabel "DataCacheTests" DataCacheTest.tests
  , TestLabel "AdoTests" $ AdoTests.tests False
  , TestLabel "OutgoneFetchesTest" OutgoneFetchesTests.tests
  , TestLabel "ProfileTests" ProfileTests.tests
  , TestLabel "MemoizationTests" MemoizationTests.tests
  , TestLabel "BadDataSourceTests" TestBadDataSource.tests
  , TestLabel "FullyAsyncTest" FullyAsyncTest.tests
  , TestLabel "WriteTest" WriteTests.tests
  , TestLabel "ParallelTest" ParallelTests.tests
  , TestLabel "StatsTests" StatsTests.tests
  ]
