module TestEquivDataSource (tests) where

import           Haxl.Prelude    as Haxl
import           Prelude         ()

import           Haxl.Core

import           EquivDataSource

import           Data.IORef
import           Data.List       (nub)
import           Test.HUnit

testEnv :: IO (Env ())
testEnv = do
  myState <- EquivDataSource.initGlobalState
  let st = stateSet myState stateEmpty
  initEnv st ()

tests :: Test
tests = TestList
  [ TestLabel "singleFetchTest" singleFetchTest
  , TestLabel "multiFetchTest" multiFetchTest
  ]

singleFetchTest :: Test
singleFetchTest = TestCase $ do
  env <- testEnv
  x <- runHaxl env $ mapM (getSimilar . MyData) [0, 7, 14, 21, 28]
  -- the numbers are all congruent modulo 7, so we expect one unique result for all of them
  assertEqual "unique result" 1 $ length (nub x)
  stats <- readIORef (statsRef env)
  -- ... and only one fetch
  assertEqual "fetches" 1 (numFetches stats)

multiFetchTest :: Test
multiFetchTest = TestCase $ do
  env <- testEnv
  x <- runHaxl env $ mapM (getSimilar . MyData) [0 .. 13]
  -- expect seven unique results
  assertEqual "unique result" 7 $ length (nub x)
  stats <- readIORef (statsRef env)
  -- ... in seven fetches
  assertEqual "fetches" 7 (numFetches stats)
