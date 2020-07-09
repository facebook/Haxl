-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}
module OutgoneFetchesTests (tests) where

import Haxl.Prelude as Haxl
import Prelude()

import Haxl.Core
import Haxl.DataSource.ConcurrentIO
import Haxl.Core.RequestStore (getMapFromRCMap)

import Data.IORef
import qualified Data.Map as Map
import Data.Proxy (Proxy(..))
import Data.Typeable
import Test.HUnit
import System.Timeout

import ExampleDataSource
import SleepDataSource

testEnv :: IO (Env () ())
testEnv = do
  exstate <- ExampleDataSource.initGlobalState
  sleepState <- mkConcurrentIOState
  let st = stateSet exstate $ stateSet sleepState stateEmpty
  e <- initEnv st ()
  return e { flags = (flags e) {report = 1} }
    -- report=1 to enable fetches tracking

-- A cheap haxl computation we interleave b/w the @sleep@ fetches.
wombats :: GenHaxl () () Int
wombats = length <$> listWombats 3

outgoneFetchesTest :: String -> Int -> GenHaxl () () a -> Test
outgoneFetchesTest label unfinished haxl = TestLabel label $ TestCase $ do
  env <- testEnv
  _ <- timeout (100*1000) $ runHaxl env haxl -- 100ms
  actual <- getMapFromRCMap <$> readIORef (submittedReqsRef env)
  assertEqual "fetchesMap" expected actual
  where
  expected = if unfinished == 0 then Map.empty else
    Map.singleton (dataSourceName (Proxy :: Proxy (ConcurrentIOReq Sleep))) $
      Map.singleton (typeOf1 (undefined :: ConcurrentIOReq Sleep a)) unfinished

tests :: Test
tests = TestList
  [ outgoneFetchesTest "finished" 0 $ do
      -- test that a completed datasource fetch doesn't show up in Env
      _ <- sleep 1  -- finished
      _ <- sleep 1  -- cached/finished
      _ <- sleep 1  -- cached/finished
      wombats
  , outgoneFetchesTest "unfinished" 2 $ do
      -- test that unfinished datasource fetches shows up in Env
      _ <- sleep 200 -- unfinished
      _ <- wombats
      _ <- sleep 300 -- unfinished
      _  <- wombats
      return ()
  , outgoneFetchesTest "mixed" 2 $ do
      -- test for finished/unfinished fetches from the same datasource
      _ <- sleep 1   -- finished
      _ <- sleep 200  -- unfinished
      _ <- sleep 300  -- unfinished
      return ()
  , outgoneFetchesTest "cached" 1 $ do
      -- test for cached requests not showing up twice in ReqCountMap
      _ <- sleep 200  -- unfinished
      _ <- sleep 200  -- cached/unfinished
      return ()
  , outgoneFetchesTest "unsent" 1 $
      -- test for unsent requests not showing up in ReqCountMap
      sleep 200 `andThen` sleep 300 -- second req should never be sent
  ]
