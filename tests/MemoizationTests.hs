{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module MemoizationTests (tests) where

import Data.IORef

import Test.HUnit

import Haxl.Core
import Haxl.Core.Monad (unsafeLiftIO)

import ExampleDataSource

memoSoundness :: Test
memoSoundness = TestCase $ do
  iEnv <- do
    exState <- ExampleDataSource.initGlobalState
    initEnv (stateSet exState stateEmpty) () :: IO (Env () ())

  unMemoizedWombats <- runHaxl iEnv $ listWombats 100

  (initialGet, subsequentGet) <- runHaxl iEnv $ do
    wombatsMemo <- newMemoWith (listWombats 100)
    let memoizedWombats = runMemo wombatsMemo

    initialGet <- memoizedWombats
    subsequentGet <- memoizedWombats

    return (initialGet, subsequentGet)

  assertBool "Memo Soundness 1" $ initialGet == unMemoizedWombats
  assertBool "Memo Soundness 2" $ subsequentGet == unMemoizedWombats

  let impure runCounterRef = unsafeLiftIO $ do
        modifyIORef runCounterRef succ
        readIORef runCounterRef

      initialRunCounter = 0 :: Int

  runCounterRef <- newIORef initialRunCounter

  (initialImpureGet, subsequentImpureGet) <- runHaxl iEnv $ do
    impureMemo <- newMemoWith (impure runCounterRef)
    let memoizedImpure = runMemo impureMemo

    initialImpureGet <- memoizedImpure
    subsequentImpureGet <- memoizedImpure

    return (initialImpureGet, subsequentImpureGet)

  assertBool "Memo Soundness 3" $ initialImpureGet == succ initialRunCounter
  assertBool "Memo Soundness 4" $ subsequentImpureGet == initialImpureGet

  let fMemoVal = 42 :: Int

  dependentResult <- runHaxl iEnv $ do
    fMemoRef <- newMemo
    gMemoRef <- newMemo

    let f = runMemo fMemoRef
        g = runMemo gMemoRef

    prepareMemo fMemoRef $ return fMemoVal
    prepareMemo gMemoRef $ succ <$> f

    a <- f
    b <- g
    return (a + b)

  assertBool "Memo Soundness 5" $ dependentResult == fMemoVal + succ fMemoVal

tests = TestList [TestLabel "Memo Soundness" memoSoundness]
