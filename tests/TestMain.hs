{-# LANGUAGE CPP, OverloadedStrings #-}
module Main where

import Test.HUnit
import AllTests

main :: IO Counts
main = runTestTT allTests
