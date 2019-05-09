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
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Proxy (Proxy(..))
import Data.Typeable
import Test.HUnit
import System.Timeout

import ExampleDataSource
import SleepDataSource

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

getFetches :: Env () () -> IO (Map Text (Map TypeRep Int))
getFetches env = getMapFromRCMap <$> readIORef (submittedReqsRef env)

outgoneFetchesTest :: Test
outgoneFetchesTest = TestCase $ do
  let
    withTimeout env h = timeout 2000 $ runHaxl env h -- 2 ms

  -- test that a completed datasource fetch doesn't show up in Env
  env <- testEnv

  withTimeout env $ do
    _ <- sleep 1 -- 1 ms
    _ <- sleep 1 -- should be cached
    _ <- sleep 1
    wombats

  fetchesMap <- getFetches env
  assertEqual "outgoneFetches1" 0 (Map.size fetchesMap)

  -- test that unfinished datasource fetches shows up in Env
  env <- testEnv

  withTimeout env $ do
    _ <- sleep 4 -- 4 ms
    _ <- wombats
    _ <- sleep 5 -- 4 ms
    _  <- wombats
    return ()

  fetchesMap <- getFetches env
  assertEqual "outgoneFetches2" 1 (Map.size fetchesMap)
  assertEqual "outgoneFetches2"
    (Map.fromList
      [ ( dataSourceName (Proxy :: Proxy (ConcurrentIOReq Sleep))
        , Map.fromList [(typeOf1 (undefined :: ConcurrentIOReq Sleep a), 2)]
        )
      ])
    fetchesMap

  -- test for finished/unfinished fetches from the same datasource
  env <- testEnv

  withTimeout env $ do
    _ <- sleep 1 -- 1 ms
    _ <- sleep 4
    _ <- sleep 5
    return ()

  fetchesMap <- getFetches env
  assertEqual "outgoneFetches3"
    (Map.fromList
      [ ( dataSourceName (Proxy :: Proxy (ConcurrentIOReq Sleep))
        , Map.fromList [(typeOf1 (undefined :: ConcurrentIOReq Sleep a), 2)]
        )
      ])
    fetchesMap

  -- test for cached requests not showing up twice in ReqCountMap
  env <- testEnv

  withTimeout env $ do
    _ <- sleep 4 -- 3 ms
    _ <- sleep 4
    return ()

  fetchesMap <- getFetches env
  assertEqual "outgoneFetches4"
    (Map.fromList
      [ ( dataSourceName (Proxy :: Proxy (ConcurrentIOReq Sleep))
        , Map.fromList [(typeOf1 (undefined :: ConcurrentIOReq Sleep a), 1)]
        )
      ])
    fetchesMap

  -- test for unsent requests not showing up in ReqCountMap
  env <- testEnv

  withTimeout env $ do
    _ <- sleep =<< sleep 4 -- second req should never be sent
    return ()

  fetchesMap <- getFetches env
  assertEqual "outgoneFetches5"
    (Map.fromList
      [ ( dataSourceName (Proxy :: Proxy (ConcurrentIOReq Sleep))
        , Map.fromList [(typeOf1 (undefined :: ConcurrentIOReq Sleep a), 1)]
        )
      ])
    fetchesMap




tests = TestList
  [ TestLabel "outgoneFetchesTest" outgoneFetchesTest
  ]
