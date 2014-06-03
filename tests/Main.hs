{-# LANGUAGE RebindableSyntax, OverloadedStrings #-}
module Main where

import TestExampleDataSource
import BatchTests
import CoreTests
import DataCacheTest

import Data.String
import Test.HUnit

import Haxl.Prelude

main :: IO Counts
main = runTestTT $ TestList
  [ TestLabel "ExampleDataSource" TestExampleDataSource.tests
  , TestLabel "BatchTests" BatchTests.tests
  , TestLabel "CoreTests" CoreTests.tests
  , TestLabel "DataCacheTests" DataCacheTest.tests
  ]
