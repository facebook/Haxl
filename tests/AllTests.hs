{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE OverloadedStrings #-}
module AllTests (allTests) where

import TestExampleDataSource
import BatchTests
import CoreTests
import DataCacheTest
import ExceptionStackTests
import AdoTests
import OutgoneFetchesTests
import ProfileTests
import MemoizationTests
import MonadAsyncTest
import TestBadDataSource
import FullyAsyncTest
import WriteTests
import ParallelTests
import StatsTests
import DataSourceDispatchTests

import Test.HUnit

allTests :: Test
allTests = TestList
  [ TestLabel "ExampleDataSource" TestExampleDataSource.tests
  , TestLabel "BatchTests-future" $ BatchTests.tests True
  , TestLabel "BatchTests-sync" $ BatchTests.tests False
  , TestLabel "CoreTests" CoreTests.tests
  , TestLabel "DataCacheTests" DataCacheTest.tests
  , TestLabel "ExceptionStackTests" ExceptionStackTests.tests
  , TestLabel "AdoTests" $ AdoTests.tests False
  , TestLabel "OutgoneFetchesTest" OutgoneFetchesTests.tests
  , TestLabel "ProfileTests" ProfileTests.tests
  , TestLabel "MemoizationTests" MemoizationTests.tests
  , TestLabel "MonadAsyncTests" MonadAsyncTest.tests
  , TestLabel "BadDataSourceTests" TestBadDataSource.tests
  , TestLabel "FullyAsyncTest" FullyAsyncTest.tests
  , TestLabel "WriteTest" WriteTests.tests
  , TestLabel "ParallelTest" ParallelTests.tests
  , TestLabel "StatsTests" StatsTests.tests
  , TestLabel "DataSourceDispatchTests" DataSourceDispatchTests.tests
  ]
