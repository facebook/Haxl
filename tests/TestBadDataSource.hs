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

import ExampleDataSource
import BadDataSource

testEnv bg fn = do
  exstate <- ExampleDataSource.initGlobalState
  badstate <- BadDataSource.initGlobalState bg
  let st = stateSet exstate $ stateSet (fn badstate) stateEmpty
  initEnv st ()

wombats :: GenHaxl () () Int
wombats = length <$> listWombats 3

wombatsMany :: GenHaxl () () Int
wombatsMany = length <$> listWombats 7

badDataSourceTest :: Bool -> Test
badDataSourceTest bg = TestCase $ do
  -- test that a failed acquire doesn't fail the other requests
  ref <- newIORef False
  env <- testEnv bg $ \st ->
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
  env <- testEnv bg $ \st ->
    st { failDispatch = throwIO (DataSourceError "dispatch")
       , failRelease = writeIORef ref True }

  x <- runHaxl env $
        (dataFetch (FailAfter 0) + wombatsMany)
          `Haxl.catch` \DataSourceError{} -> wombats

  assertEqual "badDataSourceTest3" x 3

  -- We *should* have called release
  assertEqual "badDataSourceTest4" True =<< readIORef ref

  -- test that a failed wait is a DataSourceError
  env <- testEnv bg $ \st ->
    st { failWait = throwIO (DataSourceError "wait") }

  x <- runHaxl env $
        (dataFetch (FailAfter 0) + wombatsMany)
          `Haxl.catch` \DataSourceError{} -> wombats

  assertEqual "badDataSourceTest5" x 3

  -- We *should* have called release
  assertEqual "badDataSourceTest6" True =<< readIORef ref

  -- test that a failed release is still a DataSourceError, even
  -- though the request will have completed successfully
  env <- testEnv bg $ \st ->
    st { failRelease = throwIO (DataSourceError "release") }

  let
    -- In background fetches the scheduler might happen to process the data
    -- source result (FetchError in this case) before it processes the exception
    -- from release. So we have to allow both cases.
    releaseCatcher e
      | Just DataSourceError{} <- fromException e = wombats
      | Just FetchError{} <- fromException e =
          if bg then wombats else Haxl.throw e
      | otherwise = Haxl.throw e

  x <- runHaxl env $
        (dataFetch (FailAfter 0) + wombatsMany)
          `Haxl.catch` releaseCatcher

  assertEqual "badDataSourceTest7" x 3

  -- test that if we don't throw anything we get the result
  -- (which is a fetch error for this source)
  env <- testEnv bg id
  x <- runHaxl env $
        (dataFetch (FailAfter 0) + wombatsMany)
          `Haxl.catch` \FetchError{} -> wombats

  assertEqual "badDataSourceTest8" x 3




tests = TestList
  [ TestLabel "badDataSourceTest async" (badDataSourceTest False)
  , TestLabel "badDataSourceTest background" (badDataSourceTest True)
  ]
