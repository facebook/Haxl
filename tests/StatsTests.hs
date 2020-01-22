-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module StatsTests (tests) where

import Test.HUnit
import Data.List

import Haxl.Core

aggregateBatches :: Test
aggregateBatches = TestCase $ do
  let
    statsNoBatches = [ FetchStats { fetchDataSource = "foo"
                                  , fetchBatchSize = 7
                                  , fetchStart = 0
                                  , fetchDuration = 10
                                  , fetchSpace = 1
                                  , fetchFailures = 2
                                  , fetchBatchId = n }
                                  | n <- reverse [1..10] ++ [11..20] ]
                     ++ [ FetchCall "A" ["B"], FetchCall "C" ["D"] ]
    fetchBatch = [ FetchStats { fetchDataSource = "batch"
                              , fetchBatchSize = 1
                              , fetchStart = 100
                              , fetchDuration = 1000 * n
                              , fetchSpace = 3
                              , fetchFailures = if n <= 3 then 1 else 0
                              , fetchBatchId = 123 } | n <- [1..50] ]
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

tests = TestList [TestLabel "Aggregate Batches" aggregateBatches]
