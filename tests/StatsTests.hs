-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module StatsTests (tests) where

import Test.HUnit
import Data.List
import Data.Maybe

import Haxl.Prelude
import Haxl.Core
import Prelude()

import ExampleDataSource
import SleepDataSource
import Haxl.DataSource.ConcurrentIO

import Control.Monad (void)
import Data.IORef
import qualified Data.HashMap.Strict as HashMap

aggregateBatches :: Test
aggregateBatches = TestCase $ do
  let
    statsNoBatches = [ FetchStats { fetchDataSource = "foo"
                                  , fetchBatchSize = 7
                                  , fetchStart = 0
                                  , fetchDuration = 10
                                  , fetchSpace = 1
                                  , fetchFailures = 2
                                  , fetchIgnoredFailures = 0
                                  , fetchBatchId = n
                                  , fetchIds = [1,2] }
                                  | n <- reverse [1..10] ++ [11..20] ]
                     ++ [ FetchCall "A" ["B"] 1, FetchCall "C" ["D"] 2 ]
    fetchBatch = [ FetchStats { fetchDataSource = "batch"
                              , fetchBatchSize = 1
                              , fetchStart = 100
                              , fetchDuration = 1000 * n
                              , fetchSpace = 3
                              , fetchFailures = if n <= 3 then 1 else 0
                              , fetchIgnoredFailures = 0
                              , fetchBatchId = 123
                              , fetchIds = [fromIntegral n] } | n <- [1..50] ]
    agg (sz,bids) FetchStats{..} = (sz + fetchBatchSize, fetchBatchId:bids)
    agg _ _ = error "unexpected"
    agg' = foldl' agg (0,[])
    aggNoBatch = aggregateFetchBatches agg' (Stats statsNoBatches)
    expectedNoBatch = [(7, [n]) | n <- reverse [1..20] :: [Int]]
    aggBatch = aggregateFetchBatches agg' (Stats fetchBatch)
    expectedResultBatch = (50, [123 | _ <- [1..50] :: [Int]])
    aggInterspersedBatch =
      aggregateFetchBatches agg'
      (Stats $ intersperse (head fetchBatch) statsNoBatches)
    expectedResultInterspersed =
      (21, [123 | _ <- [1..21] :: [Int]]) : expectedNoBatch
  assertEqual "No batch has no change" expectedNoBatch aggNoBatch
  assertEqual "Batch is combined" [expectedResultBatch] aggBatch
  assertEqual
    "Grouping works as expected" expectedResultInterspersed aggInterspersedBatch

testEnv :: IO (Env () ())
testEnv = do
  -- To use a data source, we need to initialize its state:
  exstate <- ExampleDataSource.initGlobalState
  sleepState <- mkConcurrentIOState

  -- And create a StateStore object containing the states we need:
  let st = stateSet exstate (stateSet sleepState stateEmpty)

  -- Create the Env:
  env <- initEnv st ()
  return env{ flags = (flags env){
    report = setReportFlag ReportFetchStack profilingReportFlags } }


fetchIdsSync :: Test
fetchIdsSync = TestCase $ do
  env <- testEnv
  _ <- runHaxl  env $
       sequence_
       [ void $ countAardvarks "abcabc" + (length <$> listWombats 3)
       , void $ listWombats 100
       , void $ listWombats 99
       , void $ countAardvarks "BANG4" `catch` \NotFound{} -> return 123
       ]
  -- expect a single DS stat
  (Stats stats) <- readIORef (statsRef env)
  let
    fetchStats = [x | x@FetchStats{} <- stats]
  assertEqual "Only 1 batch" 1 (length fetchStats)
  let
    [stat] = fetchStats
  assertEqual "No real failures" 0 (fetchFailures stat)
  assertEqual "1 ignored failure" 1 (fetchIgnoredFailures stat)

fetchIdsBackground :: Test
fetchIdsBackground = TestCase $ do
  env <- testEnv
  _ <- runHaxl  env $
       sequence_
       [ withLabel "short" $ sleep 1
       , withLabel "long" $ sleep 500 ]

  -- make sure that with memo'ing we still preserve the stack
  _ <- runHaxl  env $ withLabel "base"
    (memo (1 :: Int) $ withLabel "child" $ sleep 102)

  _ <- runHaxl env $ withLabel "short_cached" $ sleep 1

  -- expect a single DS stat
  (Stats stats) <- readIORef (statsRef env)
  (Profile p pt _) <- readIORef (profRef env)
  let
    keyMap =
      HashMap.fromList [ (label, k) | ((label,_), k) <- HashMap.toList pt]
    revMap = HashMap.fromList [(v,k) | (k,v) <- HashMap.toList pt]
    parentMap =
      HashMap.fromList $
      catMaybes
      [ case HashMap.lookup kp revMap of
          Just (lp,_) -> Just (label, lp)
          Nothing -> Nothing
      | ((label,kp), _) <- HashMap.toList pt]
    fetchMap =  HashMap.fromList [ (fid, x) | x@FetchStats{} <- stats
                                   , fid <- fetchIds x]
    get l = [ (prof, wasCached, fetchStat)
            | Just key <- [HashMap.lookup l keyMap]
            , Just prof <- [HashMap.lookup key p]
            , ProfileFetch fid _ wasCached <- profileFetches prof
            , Just fetchStat <- [HashMap.lookup fid fetchMap]]
    [(short, shortWC, shortFetch)] = get "short"
    [(long, longWC, longFetch)] = get "long"
    [(shortCached, shortCachedWC, shortCachedFetch)] = get "short_cached"

  assertEqual "3 batches" 3 (HashMap.size fetchMap)
  assertEqual "6 labels (inc MAIN)" 6 (HashMap.size keyMap)

  assertEqual "child parent is base"
    (Just "base")
    (HashMap.lookup "child" parentMap)

  assertEqual "base parent is MAIN"
    (Just "MAIN")
    (HashMap.lookup "base" parentMap)

  assertEqual "long parent is MAIN"
    (Just "MAIN")
    (HashMap.lookup "long" parentMap)

  assertBool "original fetches not cached (short)" (not shortWC)
  assertBool "original fetches not cached (long)" (not longWC)
  assertBool "was cached short" shortCachedWC

  assertEqual "one fetch short" 1 (length $ profileFetches short)
  assertEqual "one fetch long" 1 (length $ profileFetches long)
  assertEqual "one fetch short_cached" 1 (length $ profileFetches shortCached)

  assertBool "short fetch mapped properly" (fetchDuration shortFetch < 100000)
  assertEqual
    "short cached fetch mapped properly"
    (fetchDuration shortFetch)
    (fetchDuration shortCachedFetch)
  assertBool "long fetch was mapped properly" (fetchDuration longFetch > 100000)


ppStatsTest :: Test
ppStatsTest = TestCase $ do
  let
    r = ppStats (Stats [])
    mc = ppStats (Stats [MemoCall 0 0])
    fc = ppStats (Stats [FetchCall "" [] 0])
    fw = ppStats (Stats [FetchWait HashMap.empty 0 1])
    fs = ppStats (Stats [FetchStats "" 0 0 0 0 0 0 0 []])
  assertEqual "empty stats -> empty string" r ""
  assertEqual "memo call stats -> empty string" mc ""
  assertEqual "fetch call stats -> empty string" fc ""
  assertBool "fetch wait stats -> some data" (not $ null fw)
  assertBool "fetch stats -> some data" (not $ null fs)


tests = TestList [ TestLabel "Aggregate Batches" aggregateBatches
                 , TestLabel "Fetch IDs Sync" fetchIdsSync
                 , TestLabel "Fetch IDs Background" fetchIdsBackground
                 , TestLabel "ppStats" ppStatsTest ]
