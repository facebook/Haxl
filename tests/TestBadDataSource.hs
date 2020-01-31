-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE OverloadedStrings #-}
module TestBadDataSource (tests) where

import Haxl.Prelude as Haxl
import Prelude()

import Haxl.Core

import Data.IORef
import Test.HUnit
import Control.Exception
import System.Mem

import ExampleDataSource
import BadDataSource

testEnv impl fn = do
  -- Use allocation limits, just to make sure haxl properly behaves and
  -- doesn't reset this internally somewhere.
  -- `go` will disable this
  setAllocationCounter 5000000 -- 5 meg should be enough, uses ~100k atm
  enableAllocationLimit
  exstate <- ExampleDataSource.initGlobalState
  badstate <- BadDataSource.initGlobalState impl
  let st = stateSet exstate $ stateSet (fn badstate) stateEmpty
  initEnv st ()

wombats :: GenHaxl () () Int
wombats = length <$> listWombats 3

wombatsMany :: GenHaxl () () Int
wombatsMany = length <$> listWombats 7

go :: FetchImpl -> Test
go impl = TestCase $ flip finally disableAllocationLimit $ do
  -- test that a failed acquire doesn't fail the other requests
  ref <- newIORef False
  env <- testEnv impl $ \st ->
    st { failAcquire = throwIO (DataSourceError "acquire")
       , failRelease = writeIORef ref True }

  x <- runHaxl env $
        (dataFetch (FailAfter 0) + wombatsMany)
          `Haxl.catch` \DataSourceError{} -> wombats

  assertEqual "badDataSourceTest1" 3 x

  -- We should *not* have called release
  assertEqual "badDataSourceTest2" False =<< readIORef ref

  -- test that a failed dispatch doesn't fail the other requests
  ref <- newIORef False
  env <- testEnv impl $ \st ->
    st { failDispatch = throwIO (DataSourceError "dispatch")
       , failRelease = writeIORef ref True }

  x <- runHaxl env $
        (dataFetch (FailAfter 0) + wombatsMany)
          `Haxl.catch` \DataSourceError{} -> wombats

  assertEqual "badDataSourceTest3" x 3

  -- We *should* have called release
  assertEqual "badDataSourceTest4" True =<< readIORef ref

  -- test that a failed wait is a DataSourceError
  env <- testEnv impl $ \st ->
    st { failWait = throwIO (DataSourceError "wait") }

  x <- runHaxl env $
        (dataFetch (FailAfter 0) + wombatsMany)
          `Haxl.catch` \DataSourceError{} -> wombats

  assertEqual "badDataSourceTest5" x 3

  -- We *should* have called release
  assertEqual "badDataSourceTest6" True =<< readIORef ref

  -- test that a failed release is still a DataSourceError, even
  -- though the request will have completed successfully
  env <- testEnv impl $ \st ->
    st { failRelease = throwIO (DataSourceError "release") }

  let
    -- In background fetches the scheduler might happen to process the data
    -- source result (FetchError in this case) before it processes the exception
    -- from release. So we have to allow both cases.
    isBg = case impl of
      Background -> True
      BackgroundMVar -> True
      _ -> False
    releaseCatcher e
      | Just DataSourceError{} <- fromException e = wombats
      | Just FetchError{} <- fromException e =
          if isBg then wombats else Haxl.throw e
      | otherwise = Haxl.throw e

  x <- runHaxl env $
        (dataFetch (FailAfter 0) + wombatsMany)
          `Haxl.catch` releaseCatcher

  assertEqual "badDataSourceTest7" x 3

  -- test that if we don't throw anything we get the result
  -- (which is a fetch error for this source)
  env <- testEnv impl id
  x <- runHaxl env $
        (dataFetch (FailAfter 0) + wombatsMany)
          `Haxl.catch` \FetchError{} -> wombats

  assertEqual "badDataSourceTest8" x 3




tests = TestList
  [ TestLabel "badDataSourceTest async" (go Async)
  , TestLabel "badDataSourceTest background" (go Background)
  , TestLabel "badDataSourceTest backgroundMVar" (go BackgroundMVar)
  ]
