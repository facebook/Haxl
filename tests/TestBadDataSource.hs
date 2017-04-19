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

testEnv fn = do
  exstate <- ExampleDataSource.initGlobalState
  badstate <- BadDataSource.initGlobalState
  let st = stateSet exstate $ stateSet (fn badstate) stateEmpty
  initEnv st ()

wombats :: GenHaxl () Int
wombats = length <$> listWombats 3

badDataSourceTest :: Test
badDataSourceTest = TestCase $ do
  -- test that a failed acquire doesn't fail the other requests
  ref <- newIORef False
  env <- testEnv $ \st ->
    st { failAcquire = throwIO (DataSourceError "acquire")
       , failRelease = writeIORef ref True }

  x <- runHaxl env $
        (dataFetch (FailAfter 0) + wombats)
          `Haxl.catch` \DataSourceError{} -> wombats

  assertEqual "badDataSourceTest1" 3 x

  -- We should *not* have called release
  assertEqual "badDataSourceTest2" False =<< readIORef ref

  -- test that a failed dispatch doesn't fail the other requests
  ref <- newIORef False
  env <- testEnv $ \st ->
    st { failDispatch = throwIO (DataSourceError "dispatch")
       , failRelease = writeIORef ref True }

  x <- runHaxl env $
        (dataFetch (FailAfter 0) + wombats)
          `Haxl.catch` \DataSourceError{} -> wombats

  assertEqual "badDataSourceTest3" x 3

  -- We *should* have called release
  assertEqual "badDataSourceTest4" True =<< readIORef ref

  -- test that a failed wait is a DataSourceError
  env <- testEnv $ \st ->
    st { failWait = throwIO (DataSourceError "wait") }

  x <- runHaxl env $
        (dataFetch (FailAfter 0) + wombats)
          `Haxl.catch` \DataSourceError{} -> wombats

  assertEqual "badDataSourceTest5" x 3

  -- We *should* have called release
  assertEqual "badDataSourceTest6" True =<< readIORef ref

  -- test that a failed release is still a DataSourceError, even
  -- though the request will have completed successfully
  env <- testEnv $ \st ->
    st { failRelease = throwIO (DataSourceError "release") }

  x <- runHaxl env $
        (dataFetch (FailAfter 0) + wombats)
          `Haxl.catch` \DataSourceError{} -> wombats

  assertEqual "badDataSourceTest7" x 3




tests = TestList
  [ TestLabel "badDataSourceTest" badDataSourceTest
  ]
