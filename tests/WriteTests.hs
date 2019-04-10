-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE CPP, OverloadedStrings #-}
module WriteTests (tests) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import Test.HUnit

import Data.Foldable

import Haxl.Core
import Haxl.Prelude as Haxl

newtype SimpleWrite = SimpleWrite Text
  deriving (Eq, Show)

doWrite :: GenHaxl u SimpleWrite Int
doWrite = do
  tellWrite $ SimpleWrite "inner"
  return 0

doOuterWrite :: GenHaxl u SimpleWrite Int
doOuterWrite = do
  tellWrite $ SimpleWrite "outer1"

  doWriteMemo <- newMemoWith doWrite
  let doMemoizedWrite = runMemo doWriteMemo
  _ <- doMemoizedWrite
  _ <- doMemoizedWrite

  tellWrite $ SimpleWrite "outer2"

  return 1

writeSoundness :: Test
writeSoundness = TestCase $ do
  let numReps = 4

  -- do writes without memoization
  env1 <- emptyEnv ()
  (allRes, allWrites) <- runHaxlWithWrites env1 $
    Haxl.sequence (replicate numReps doWrite)

  assertBool "Write Soundness 1" $
    allWrites == replicate numReps (SimpleWrite "inner")
  assertBool "Write Soundness 2" $ allRes == replicate numReps 0

  -- do writes with memoization
  env2 <- emptyEnv ()

  (memoRes, memoWrites) <- runHaxlWithWrites env2 $ do
    doWriteMemo <- newMemoWith doWrite
    let memoizedWrite = runMemo doWriteMemo

    Haxl.sequence (replicate numReps memoizedWrite)

  assertBool "Write Soundness 3" $
    memoWrites == replicate numReps (SimpleWrite "inner")
  assertBool "Write Soundness 4" $ memoRes == replicate numReps 0

  -- do writes with interleaved memo
  env3 <- emptyEnv ()

  (ilRes, ilWrites) <- runHaxlWithWrites env3 $ do
    doWriteMemo <- newMemoWith doWrite
    let memoizedWrite = runMemo doWriteMemo

    Haxl.sequence $ replicate numReps (doWrite *> memoizedWrite)

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

tests = TestList [TestLabel "Write Soundness" writeSoundness]
