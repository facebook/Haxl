-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE OverloadedStrings #-}
module WriteTests (tests) where

import Test.HUnit

import Data.Foldable

import Haxl.Core
import Haxl.Prelude as Haxl

newtype SimpleWrite = SimpleWrite Text
  deriving (Eq, Show)

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



tests = TestList [TestLabel "Write Soundness" writeSoundness]
