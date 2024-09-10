{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE CPP, OverloadedStrings #-}
module Main where

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import AllTests

main :: IO ()
main = defaultMain $ hUnitTestToTests allTests
