{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module FBMain (main) where

import Facebook.Init
import TestRunner
import AllTests

main :: IO ()
main = withFacebookUnitTest $ testRunner $ allTests
