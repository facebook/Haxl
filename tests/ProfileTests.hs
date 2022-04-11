-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

module ProfileTests where

import Haxl.Prelude

import Haxl.Core
import Haxl.Core.Monad
import Haxl.Core.Stats

import Test.HUnit

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Aeson
import Data.IORef
import qualified Data.HashMap.Strict as HashMap
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap as KeyMap
#else
import qualified Data.HashMap.Strict as KeyMap
#endif
import Data.Int

import TestUtils
import WorkDataSource
import SleepDataSource

mkProfilingEnv = do
  env <- makeTestEnv False
  return env { flags = (flags env) { report = profilingReportFlags } }

-- expects only one label to be shown
labelToDataMap :: Profile -> HashMap.HashMap ProfileLabel ProfileData
labelToDataMap Profile{..} = HashMap.fromList hashKeys
  where
    labelKeys = HashMap.fromList [
      (k, l) | ((l, _), k) <- HashMap.toList profileTree]
    hashKeys = [ (l, v)
      | (k, v) <- HashMap.toList profile
      , Just l <- [HashMap.lookup k labelKeys]]

collectsdata :: Assertion
collectsdata = do
  e <- mkProfilingEnv
  _x <- runHaxl e $
          withLabel "bar" $
            withLabel "foo" $ do
              u <- env userEnv
              slp <- sum <$> mapM (\x -> withLabel "baz" $ return x) [1..5]
              -- do some non-trivial work that can't be lifted out
              -- first sleep though in order to force a Blocked result
              sleep slp `andThen` case fromJSON <$> KeyMap.lookup "A" u of
                Just (Success n) | sum [n .. 1000::Integer] > 0 -> return 5
                _otherwise -> return (4::Int)
  profCopy <- readIORef (profRef e)
  let
    profData = profile profCopy
    labelKeys = HashMap.fromList [
      (l, k) | ((l, _), k) <- HashMap.toList (profileTree profCopy)]
    getData k = do
      k2 <- HashMap.lookup k labelKeys
      HashMap.lookup k2 profData
  assertEqual "has data" 4 $ HashMap.size profData
  assertBool "foo allocates" $
    case profileAllocs <$> getData "foo" of
      Just x -> x > 10000
      Nothing -> False
  assertEqual "foo is only called once" (Just 1) $
    profileLabelHits <$> getData "foo"
  assertEqual "baz is called 5 times" (Just 5) $
    profileLabelHits <$> getData "baz"
  assertBool "bar does not allocate (much)" $
    case profileAllocs <$> getData "bar" of
      Just n -> n < 5000  -- getAllocationCounter can be off by +/- 4K
      _otherwise -> False
  let fooParents = case HashMap.lookup "foo" labelKeys of
        Nothing -> []
        Just kfoo ->
          [ kparent
          | ((_, kparent), k) <- HashMap.toList (profileTree profCopy)
          , k == kfoo]
  assertEqual "foo's parent" 1 (length fooParents)
  assertEqual "foo's parent is bar" (Just (head fooParents)) $
    HashMap.lookup ("bar", 0) (profileTree profCopy)


collectsLazyData :: Assertion
collectsLazyData = do
  e <- mkProfilingEnv
  _x <- runHaxl e $ withLabel "bar" $ do
          u <- env userEnv
          withLabel "foo" $ do
             let start = if KeyMap.member "A" u
                         then 10
                         else 1
             return $ sum [start..10000::Integer]
  profCopy <- readIORef (profRef e)
  -- check the allocations are attributed to foo
  assertBool "foo has allocations" $
    case profileAllocs <$> HashMap.lookup "foo" (labelToDataMap profCopy) of
      Just x -> x > 10000
      Nothing -> False

exceptions :: Assertion
exceptions = do
  env <- mkProfilingEnv
  _x <- runHaxl env $
          withLabel "outer" $
            tryToHaxlException $ withLabel "inner" $
              unsafeLiftIO $ evaluate $ force (error "pure exception" :: Int)
  profData <- labelToDataMap <$> readIORef (profRef env)
  assertBool "inner label not added" $
    not $ HashMap.member "inner" profData

  env2 <- mkProfilingEnv
  _x <- runHaxl env2 $
          withLabel "outer" $
            tryToHaxlException $ withLabel "inner" $
              throw $ NotFound "haxl exception"
  profData <- labelToDataMap <$> readIORef (profRef env2)
  assertBool "inner label added" $
    HashMap.member "inner" profData


-- Test that we correctly attribute work done in child threads when
-- using BackgroundFetch to the caller of runHaxl. This is important
-- for correct accounting when relying on allocation limits.
threadAlloc :: Integer -> Assertion
threadAlloc batches = do
  env' <- initEnv (stateSet mkWorkState stateEmpty) ()
  let env = env'  { flags = (flags env') {
    report = setReportFlag ReportFetchStats defaultReportFlags } }
  a0 <- getAllocationCounter
  let
    wsize = 100000
    w = forM [wsize..(wsize+batches-1)] work
  _x <- runHaxl env $ sum <$> w
  a1 <- getAllocationCounter
  let
    lower = fromIntegral $ 1000000 * batches
    upper = fromIntegral $ 25000000 * batches
  assertBool "threadAlloc lower bound" $ (a0 - a1) > lower
  assertBool "threadAlloc upper bound" $ (a0 - a1) < upper
    -- the result was 16MB on 64-bit, or around 25KB if we miss the allocs
    -- in the child thread. For batched it should be similarly scaled.
    -- When we do not reset the counter for each batch was
    -- scaled again by number of batches.

  stats <- readIORef (statsRef env)
  assertEqual
    "threadAlloc: batches"
    [fromIntegral batches]
    (aggregateFetchBatches length stats)
  -- if we actually do more than 1 batch then the above test is not useful

data MemoType = Global | Local

-- Test that we correctly attribute memo work
memos:: MemoType -> Assertion
memos memoType = do
  env <- mkProfilingEnv
  let
    memoAllocs = 10000000 :: Int64
    doWork = unsafeLiftIO $ do
      a0 <- getAllocationCounter
      setAllocationCounter $ a0 - memoAllocs
      return (5 :: Int)
    mkWork
      | Global <- memoType = return (memo (1 :: Int) doWork)
      | Local <- memoType = memoize doWork
  _ <- runHaxl env $ do
    work <- mkWork
    andThen
      (withLabel "do" work)
      (withLabel "cached" work)
  profData <- labelToDataMap <$> readIORef (profRef env)
  case HashMap.lookup "do" profData of
    Nothing -> assertFailure "do not in data"
    Just ProfileData{..} -> do
      assertEqual "has correct memo id" profileMemos [ProfileMemo 1 False]
      assertBool "allocs are included in 'do'" (profileAllocs >= memoAllocs)
  case HashMap.lookup "cached" profData of
    Nothing -> assertFailure "cached not in data"
    Just ProfileData{..} -> do
      assertEqual "has correct memo id" profileMemos [ProfileMemo 1 True]
      assertBool "allocs are *not* included in 'cached'" (profileAllocs < 50000)
  (Stats memoStats) <- readIORef (statsRef env)
  assertEqual "exactly 1 memo/fetch" 1 (length memoStats)
  let memoStat = head memoStats
  putStrLn $ "memoStat=" ++ show memoStat
  assertEqual "correct call id" 1 (memoStatId memoStat)
  assertBool "allocs are big enough" $ memoSpace memoStat >= memoAllocs
  assertBool "allocs are not too big" $ memoSpace memoStat < memoAllocs + 100000


tests = TestList
  [ TestLabel "collectsdata" $ TestCase collectsdata
  , TestLabel "collectsdata - lazy" $ TestCase collectsLazyData
  , TestLabel "exceptions" $ TestCase exceptions
  , TestLabel "threads" $ TestCase (threadAlloc 1)
  , TestLabel "threads with batch" $ TestCase (threadAlloc 50)
  , TestLabel "memos - Global" $ TestCase (memos Global)
  , TestLabel "memos - Local" $ TestCase (memos Local)
  ]
