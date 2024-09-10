{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}

module DataSourceDispatchTests (tests) where
import Test.HUnit hiding (State)
import Control.Monad
import Haxl.Core
import Data.Hashable

data DataSourceDispatch ty where
    GetBatchSize :: Int -> DataSourceDispatch Int

deriving instance Eq (DataSourceDispatch ty)
deriving instance Show (DataSourceDispatch ty)

instance DataSourceName DataSourceDispatch where
    dataSourceName _ = "DataSourceDispatch"

instance StateKey DataSourceDispatch where
    data State DataSourceDispatch = DataSourceDispatchState

instance ShowP DataSourceDispatch where showp = show

instance Hashable (DataSourceDispatch a) where
  hashWithSalt s (GetBatchSize n) = hashWithSalt s n

initDataSource :: IO (State DataSourceDispatch)
initDataSource = return DataSourceDispatchState

instance DataSource UserEnv DataSourceDispatch where
    fetch _state _flags _u = SyncFetch $ \bfs -> forM_ bfs (fill $ length bfs)
      where
      fill :: Int -> BlockedFetch DataSourceDispatch -> IO ()
      fill l (BlockedFetch (GetBatchSize _ ) rv) = putResult rv (Right l)

    schedulerHint Batching = TryToBatch
    schedulerHint NoBatching = SubmitImmediately

data UserEnv = Batching | NoBatching deriving (Eq)

makeTestEnv :: UserEnv -> IO (Env UserEnv ())
makeTestEnv testUsrEnv = do
  st <- initDataSource
  e <- initEnv (stateSet st stateEmpty) testUsrEnv
  return e { flags = (flags e) {
    report = setReportFlag ReportFetchStats defaultReportFlags } }

schedulerTest:: Test
schedulerTest = TestCase $ do
    let
        fet = do
          x <- dataFetch (GetBatchSize 0)
          y <- dataFetch (GetBatchSize 1)
          return [x,y]

    e <- makeTestEnv Batching
    r1 :: [Int] <- runHaxl e fet
    assertEqual "Failed to create batches for data fetch" [2,2] r1

    eNoBatching <- makeTestEnv NoBatching
    r2 :: [Int] <- runHaxl eNoBatching fet
    assertEqual "Unexpexted batches in SubmitImmediately" [1,1] r2

    return ()

tests :: Test
tests = TestList
  [ TestLabel "schedulerTest" schedulerTest
  ]
