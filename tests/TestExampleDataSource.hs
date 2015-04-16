{-# LANGUAGE OverloadedStrings, RebindableSyntax, MultiWayIf #-}
module TestExampleDataSource (tests) where

import Haxl.Prelude as Haxl
import Prelude()

import Haxl.Core.Monad (unsafeLiftIO)
import Haxl.Core

import qualified Data.HashMap.Strict as HashMap
import Test.HUnit
import Data.IORef
import Control.Exception

import ExampleDataSource
import LoadCache

testEnv = do
  -- To use a data source, we need to initialize its state:
  exstate <- ExampleDataSource.initGlobalState

  -- And create a StateStore object containing the states we need:
  let st = stateSet exstate stateEmpty

  -- Create the Env:
  initEnv st ()


tests = TestList [
  TestLabel "exampleTest" exampleTest,
  TestLabel "orderTest" orderTest,
  TestLabel "preCacheTest" preCacheTest,
  TestLabel "cachedComputationTest" cachedComputationTest,
  TestLabel "memoTest" memoTest,
  TestLabel "dataSourceExceptionTest" dataSourceExceptionTest,
  TestLabel "dumpCacheAsHaskell" dumpCacheTest]

-- Let's test ExampleDataSource.

exampleTest :: Test
exampleTest = TestCase $ do
  env <- testEnv

  -- Run an example expression with two fetches:
  x <- runHaxl env $
     countAardvarks "abcabc" + (length <$> listWombats 3)

  assertEqual "runTests" x (2 + 3)

  -- Should be just one fetching round:
  Stats stats <- readIORef (statsRef env)
  assertEqual "rounds" 1 (length stats)

  -- With two fetches:
  assertBool "reqs" $
      if | RoundStats { roundDataSources = m } : _  <- stats,
           Just (DataSourceRoundStats { dataSourceFetches = 2 })
              <- HashMap.lookup "ExampleDataSource" m  -> True
         | otherwise -> False

-- Test side-effect ordering

orderTest = TestCase $ do
  env <- testEnv

  ref <- newIORef ([] :: [Int])

  let tick n = unsafeLiftIO (modifyIORef ref (n:))

  let left = do tick 1
                r <- countAardvarks "abcabc"
                tick 2
                return r

  let right = do tick 3
                 r <- length <$> listWombats 3
                 tick 4
                 return r

  x <- runHaxl env $ left + right
  assertEqual "TestExampleDataSource2" x (2 + 3)

  -- The order of the side effects is 1,3,2,4.  First we see 1, then
  -- left gets blocked, then we explore right, we see 3, then right
  -- gets blocked.  The data fetches are performed, then we see 2 and
  -- then 4.

  ys <- readIORef ref
  assertEqual "TestExampleDataSource: ordering" (reverse ys) [1,3,2,4]


preCacheTest = TestCase $ do
  env <- testEnv

  x <- runHaxl env $ do
    cacheRequest (CountAardvarks "xxx") (Right 3)
    cacheRequest (ListWombats 100) (Right [1,2,3])
    countAardvarks "xxx" + (length <$> listWombats 100)
  assertEqual "preCacheTest1" x (3 + 3)

  y <- Control.Exception.try $ runHaxl env $ do
    cacheRequest (CountAardvarks "yyy") $ except (NotFound "yyy")
    countAardvarks "yyy"
  assertBool "preCacheTest2" $
     case y of
       Left (NotFound "yyy") -> True
       _other -> False

-- Pretend CountAardvarks is a request computed by some Haxl code
cachedComputationTest = TestCase $ do
  env <- testEnv
  let env' = env { flags = (flags env){trace = 3} }

  let x = cachedComputation (CountAardvarks "ababa") $ do
        a <- length <$> listWombats 10
        b <- length <$> listWombats 20
        return (a + b)

  r <- runHaxl env' $ x + x + countAardvarks "baba"

  assertEqual "cachedComputationTest1" 62 r

  stats <- readIORef (statsRef env)
  assertEqual "fetches" 3 (numFetches stats)

-- Pretend CountAardvarks is a request computed by some Haxl code
memoTest = TestCase $ do
  env <- testEnv
  let env' = env { flags = (flags env){trace = 3} }

  let x = memo (CountAardvarks "ababa") $ do
        a <- length <$> listWombats 10
        b <- length <$> listWombats 20
        return (a + b)

  r <- runHaxl env' $ x + x + countAardvarks "baba"

  assertEqual "memoTest1" 62 r

  stats <- readIORef (statsRef env)
  assertEqual "fetches" 3 (numFetches stats)

dataSourceExceptionTest = TestCase $ do
  env <- testEnv

  r <- runHaxl env $ Haxl.try $ countAardvarks "BANG"
  assertBool "exception1" $
    case r of
      Left (ErrorCall "BANG") -> True
      _ -> False
  r <- runHaxl env $ Haxl.try $ countAardvarks "BANG2"
  assertBool "exception2" $
    case r of
      Left (ErrorCall "BANG2") -> True
      _ -> False

  -- In this test, BANG3 is an asynchronous exception (ThreadKilled),
  -- so we should see that instead of the exception on the left.
  -- Furthermore, it doesn't get caught by Haxl.try, and we have to
  -- catch it outside of runHaxl.
  env <- testEnv
  r <- Control.Exception.try $ runHaxl env $ Haxl.try $
          (length <$> listWombats 100) + countAardvarks "BANG3"
  print r
  assertBool "exception3" $
    case (r :: Either AsyncException (Either SomeException Int)) of
       Left ThreadKilled -> True
       _ -> False

-- Test that we can load the cache from a dumped copy of it, and then dump it
-- again to get the same result.
dumpCacheTest = TestCase $ do
  env <- testEnv
  runHaxl env loadCache
  str <- runHaxl env dumpCacheAsHaskell
  loadcache <- readFile "sigma/haxl/core/tests/LoadCache.txt"
  assertEqual "dumpCacheAsHaskell" str loadcache
