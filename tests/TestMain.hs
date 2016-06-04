{-# LANGUAGE CPP, OverloadedStrings #-}
module Main where

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import AllTests

main :: IO ()
main = defaultMain $ hUnitTestToTests allTests
