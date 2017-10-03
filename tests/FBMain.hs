module FBMain (main) where

import Facebook.Init
import TestRunner
import AllTests

main :: IO ()
main = withFacebookUnitTest $ testRunner $ allTests
