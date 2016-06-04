{-# LANGUAGE CPP, OverloadedStrings #-}
module AllTests (allTests) where

import TestExampleDataSource
import BatchTests
import CoreTests
import DataCacheTest
#ifdef HAVE_APPLICATIVEDO
import AdoTests
#endif
#if __GLASGOW_HASKELL__ >= 710
import ProfileTests
#endif

import Test.HUnit

allTests :: Test
allTests = TestList
  [ TestLabel "ExampleDataSource" TestExampleDataSource.tests
  , TestLabel "BatchTests" BatchTests.tests
  , TestLabel "CoreTests" CoreTests.tests
  , TestLabel "DataCacheTests" DataCacheTest.tests
#ifdef HAVE_APPLICATIVEDO
  , TestLabel "AdoTests" AdoTests.tests
#endif
#if __GLASGOW_HASKELL__ >= 710
  , TestLabel "ProfileTests" ProfileTests.tests
#endif
  ]
