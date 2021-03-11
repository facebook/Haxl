-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module WriteTests (tests) where

import Test.HUnit

import Control.Concurrent
import Data.Either
import Data.Foldable
import Data.Hashable
import Data.IORef
import qualified Data.Text as Text

import Haxl.Core.Monad (mapWrites, flattenWT)
import Haxl.Core
import Haxl.Prelude as Haxl

-- A fake data source
data SimpleDataSource a where
  GetNumber :: SimpleDataSource Int

deriving instance Eq (SimpleDataSource a)
deriving instance Show (SimpleDataSource a)
instance ShowP SimpleDataSource where showp = show

instance Hashable (SimpleDataSource a) where
  hashWithSalt s GetNumber = hashWithSalt s (0 :: Int)

instance StateKey SimpleDataSource where
  data State SimpleDataSource = DSState

instance DataSourceName SimpleDataSource where
  dataSourceName _ = "SimpleDataSource"

instance DataSource u SimpleDataSource where
  fetch _st _flags _usr  = SyncFetch $ Haxl.mapM_ fetch1
    where
    fetch1 :: BlockedFetch SimpleDataSource -> IO ()
    fetch1 (BlockedFetch GetNumber m) =
      threadDelay 1000 >> putSuccess m 37

newtype SimpleWrite = SimpleWrite Text
  deriving (Eq, Show, Ord)

assertEqualIgnoreOrder ::
  (Eq a, Show a, Ord a) => String -> [a] -> [a] -> Assertion
assertEqualIgnoreOrder msg lhs rhs =
  assertEqual msg (sort lhs) (sort rhs)

doInnerWrite :: GenHaxl u SimpleWrite Int
doInnerWrite = do
  tellWrite $ SimpleWrite "inner"
  return 0

doOuterWrite :: GenHaxl u SimpleWrite Int
doOuterWrite = do
  tellWrite $ SimpleWrite "outer1"

  doWriteMemo <- newMemoWith doInnerWrite
  let doMemoizedWrite = runMemo doWriteMemo
  _ <- doMemoizedWrite
  _ <- doMemoizedWrite

  tellWrite $ SimpleWrite "outer2"

  return 1

doNonMemoWrites :: GenHaxl u SimpleWrite Int
doNonMemoWrites = do
  tellWrite $ SimpleWrite "inner"
  tellWriteNoMemo $ SimpleWrite "inner not memo"
  return 0

writeSoundness :: Test
writeSoundness = TestCase $ do
  let numReps = 4

  -- do writes without memoization
  env1 <- emptyEnv ()
  (allRes, allWrites) <- runHaxlWithWrites env1 $
    Haxl.sequence (replicate numReps doInnerWrite)

  assertBool "Write Soundness 1" $
    allWrites == replicate numReps (SimpleWrite "inner")
  assertBool "Write Soundness 2" $ allRes == replicate numReps 0

  -- do writes with memoization
  env2 <- emptyEnv ()

  (memoRes, memoWrites) <- runHaxlWithWrites env2 $ do
    doWriteMemo <- newMemoWith doInnerWrite
    let memoizedWrite = runMemo doWriteMemo

    Haxl.sequence (replicate numReps memoizedWrite)

  assertBool "Write Soundness 3" $
    memoWrites == replicate numReps (SimpleWrite "inner")
  assertBool "Write Soundness 4" $ memoRes == replicate numReps 0

  -- do writes with interleaved memo
  env3 <- emptyEnv ()

  (ilRes, ilWrites) <- runHaxlWithWrites env3 $ do
    doWriteMemo <- newMemoWith doInnerWrite
    let memoizedWrite = runMemo doWriteMemo

    Haxl.sequence $ replicate numReps (doInnerWrite *> memoizedWrite)

  assertBool "Write Soundness 5" $
    ilWrites == replicate (2*numReps) (SimpleWrite "inner")
  assertBool "Write Soundness 6" $ ilRes == replicate numReps 0

  -- do writes with nested memo
  env4 <- emptyEnv ()

  (nestRes, nestWrites) <- runHaxlWithWrites env4 $ do
    doWriteMemo' <- newMemoWith doOuterWrite
    let memoizedWrite' = runMemo doWriteMemo'

    Haxl.sequence (replicate numReps memoizedWrite')

  let expWrites =
        [ SimpleWrite "outer1"
        , SimpleWrite "inner"
        , SimpleWrite "inner"
        , SimpleWrite "outer2"
        ]
  assertBool "Write Soundness 7" $
    nestWrites == fold (replicate numReps expWrites)
  assertBool "Write Soundness 8" $ nestRes == replicate numReps 1

  -- do both kinds of writes without memoization
  env5 <- emptyEnv ()
  (allRes, allWrites) <- runHaxlWithWrites env5 $
    Haxl.sequence (replicate numReps doNonMemoWrites)

  assertBool "Write Soundness 9" $
    allWrites == replicate numReps (SimpleWrite "inner") ++
      replicate numReps (SimpleWrite "inner not memo")
  assertBool "Write Soundness 10" $ allRes == replicate numReps 0

  -- do both kinds of writes with memoization
  env6 <- emptyEnv ()

  (memoRes, memoWrites) <- runHaxlWithWrites env6 $ do
    doWriteMemo <- newMemoWith doNonMemoWrites
    let memoizedWrite = runMemo doWriteMemo

    Haxl.sequence (replicate numReps memoizedWrite)

  -- "inner not memo" only appears once in this test
  assertBool "Write Soundness 11" $
    memoWrites == replicate numReps (SimpleWrite "inner") ++
      [SimpleWrite "inner not memo"]
  assertBool "Write Soundness 12" $ memoRes == replicate numReps 0

writeLogsCorrectnessTest :: Test
writeLogsCorrectnessTest = TestLabel "writeLogs_correctness" $ TestCase $ do
  e <- emptyEnv ()
  (_ , wrts) <- runHaxlWithWrites e doNonMemoWrites
  assertEqualIgnoreOrder "Expected writes" [SimpleWrite "inner",
    SimpleWrite "inner not memo"] wrts
  wrtsNoMemo <- readIORef $ writeLogsRefNoMemo e
  wrtsMemo <- readIORef $ writeLogsRef e
  assertEqualIgnoreOrder "WriteTree not empty" [] $ flattenWT wrtsNoMemo
  assertEqualIgnoreOrder "WriteTree not empty" [] $ flattenWT wrtsMemo

mapWritesTest :: Test
mapWritesTest = TestLabel "mapWrites" $ TestCase $ do
  let func (SimpleWrite s) = SimpleWrite $ Text.toUpper s
  env0 <- emptyEnv ()
  (res0, wrts0) <- runHaxlWithWrites env0 $ mapWrites func doNonMemoWrites
  assertEqual "Expected computation result" 0 res0
  assertEqualIgnoreOrder "Writes correctly transformed" [SimpleWrite "INNER",
    SimpleWrite "INNER NOT MEMO"] wrts0

  -- Writes should behave the same inside and outside mapWrites
  env1 <- emptyEnv ()
  (res1, wrts1) <- runHaxlWithWrites env1 $ do
    outer <- doOuterWrite
    outerMapped <- mapWrites func doOuterWrite
    return $ outer == outerMapped
  assertBool "Results are identical" res1
  assertEqualIgnoreOrder
    "Writes correctly transformed, non-transformed writes preserved"
    [ SimpleWrite "outer1", SimpleWrite "inner"
    , SimpleWrite "inner", SimpleWrite "outer2"
    , SimpleWrite "OUTER1", SimpleWrite "INNER"
    , SimpleWrite "INNER", SimpleWrite "OUTER2"
    ]
    wrts1

  -- Memoization behaviour should be unaffected
  env2 <- emptyEnv ()
  (_res2, wrts2) <- runHaxlWithWrites env2 $ do
    writeMemo <- newMemoWith doNonMemoWrites
    let doWriteMemo = runMemo writeMemo
    _ <- mapWrites func doWriteMemo
    _ <- doWriteMemo
    return ()
  -- "inner not memo" should appear only once
  assertEqualIgnoreOrder
    "Write correctly transformed under memoization"
    [ SimpleWrite "INNER"
    , SimpleWrite "inner"
    , SimpleWrite "INNER NOT MEMO"
    ]
    wrts2

  -- Same as previous, but the non-mapped computation is run first
  env3 <- emptyEnv ()
  (_res3, wrts3) <- runHaxlWithWrites env3 $ do
    writeMemo <- newMemoWith doNonMemoWrites
    let doWriteMemo = runMemo writeMemo
    _ <- doWriteMemo
    _ <- mapWrites func doWriteMemo
    return ()
  -- "inner not memo" should appear only once
  assertEqualIgnoreOrder
    "Flipped: Write correctly transformed under memoization"
    [ SimpleWrite "inner"
    , SimpleWrite "INNER"
    , SimpleWrite "inner not memo"
    ]
    wrts3

  -- inner computation performs no writes
  env4 <- emptyEnv ()
  (res4, wrts4) <- runHaxlWithWrites env4 $
    mapWrites func (return (0 :: Int))
  assertEqual "No Writes: Expected computation result" 0 res4
  assertEqualIgnoreOrder "No writes" [] wrts4

  -- inner computation throws an exception
  env5 <- emptyEnv ()
  (res5, wrts5) <- runHaxlWithWrites env5 $ mapWrites func $ try $ do
    _ <- doNonMemoWrites
    _ <- throw (NotFound "exception")
    return 0
  assertBool "Throw: Expected Computation Result" $ isLeft
    (res5 :: Either HaxlException Int)
  assertEqualIgnoreOrder
    "Datasource writes correctly transformed"
    [ SimpleWrite "INNER"
    , SimpleWrite "INNER NOT MEMO"
    ]
    wrts5

  -- inner computation calls a datasource
  env6 <- initEnv (stateSet DSState stateEmpty) ()
  (res6, wrts6) <- runHaxlWithWrites env6 $ mapWrites func $ do
    _ <- doNonMemoWrites
    dataFetch GetNumber

  assertEqual "Datasource: Expected Computation Result" 37 res6
  assertEqualIgnoreOrder
    "Datasource writes correctly transformed"
    [ SimpleWrite "INNER"
    , SimpleWrite "INNER NOT MEMO"
    ]
    wrts6

  -- inner computation calls a datasource, flipped calls
  env7 <- initEnv (stateSet DSState stateEmpty) ()
  (res7, wrts7) <- runHaxlWithWrites env7 $ mapWrites func $ do
    df <- dataFetch GetNumber
    _ <- doNonMemoWrites
    return df

  assertEqual "Flipped Datasource: Expected Computation Result" 37 res7
  assertEqualIgnoreOrder
    "Flipped: Datasource writes correctly transformed"
    [ SimpleWrite "INNER"
    , SimpleWrite "INNER NOT MEMO"
    ]
    wrts7

tests = TestList
  [ TestLabel "Write Soundness" writeSoundness,
    TestLabel "writeLogs_correctness" writeLogsCorrectnessTest,
    TestLabel "mapWrites" mapWritesTest
  ]
