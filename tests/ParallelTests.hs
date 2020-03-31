module ParallelTests where

import Haxl.Prelude
import Haxl.Core

import Haxl.DataSource.ConcurrentIO
import SleepDataSource

import Data.Time.Clock

import Test.HUnit

testEnv = do
  sleepState <- mkConcurrentIOState
  let st = stateSet sleepState stateEmpty
  initEnv st ()

sync_test :: IO ()
sync_test = do
  env <- testEnv
  -- This computation tests that the two arguments of the pOr can fire
  -- without causing an error. The reason we test for this is that the
  -- synchronization involved in this case is a little fragile.
  False <- runHaxl env $ do
    (fmap (const False) (sleep 50)
      `pOr` fmap (const False) (sleep 100))
      `pOr` fmap (const False) (sleep 200)
  return ()

semantics_when_computation_is_blocked_test :: IO ()
semantics_when_computation_is_blocked_test = do
  env <- testEnv
    -- Test semantics of blocking
  let sleepReturn bool t = do
        _ <- sleep t
        return bool
  r <- runHaxl env $ do
        -- All sleep times are different so that they're not cached
        a <- sleepReturn False 10 `pOr` sleepReturn False 11
        b <- sleepReturn False 12 `pOr` sleepReturn True 13
        c <- sleepReturn True 14 `pOr` sleepReturn False 15
        d <- sleepReturn True 16 `pOr` sleepReturn True 17
        return (not a && b && c && d)
  assertBool "pOr blocked semantics" r


timing_test = do
  env <- testEnv
  t0 <- getCurrentTime
  True <- runHaxl env $
    fmap (const True) (sleep 200) `pOr` fmap (const True) (sleep 100)
  t1 <- getCurrentTime
  True <- runHaxl env $
    fmap (const True) (sleep 100) `pOr` fmap (const True) (sleep 200)
  t2 <- getCurrentTime
  False <- runHaxl env $
    fmap (const False) (sleep 200) `pOr` fmap (const False) (sleep 100)
  t3 <- getCurrentTime
  False <- runHaxl env $
    fmap (const False) (sleep 100) `pOr` fmap (const False) (sleep 200)
  t4 <- getCurrentTime
  -- diffUTCTime returns the difference in seconds,
  -- while sleep expects milliseconds
  assert (t4 `diffUTCTime` t3 < 0.2)
  assert (t3 `diffUTCTime` t2 < 0.2)
  assert (t2 `diffUTCTime` t1 < 0.2)
  assert (t1 `diffUTCTime` t0 < 0.2)

tests = TestList [TestLabel "sync_test" (TestCase sync_test)
                 ,TestLabel "timing_test" (TestCase timing_test)
                 ,TestLabel "semantics_when_computation_is_blocked_test" (TestCase semantics_when_computation_is_blocked_test)
                 ]
