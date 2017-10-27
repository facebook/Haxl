-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file. An additional grant of patent rights can
-- be found in the PATENTS file.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module SleepDataSource (
    -- * initialise the state
    initGlobalState,

    -- * requests for this data source
    SleepReq(..),
    sleep,
  ) where

import Haxl.Prelude
import Prelude ()

import Haxl.Core

import Control.Monad hiding (mapM_)
import Data.Typeable
import Data.Hashable
import Control.Concurrent

sleep :: Int -> GenHaxl u Int
sleep n = dataFetch (Sleep n)

data SleepReq a where
  Sleep :: Int -> SleepReq Int
  deriving Typeable -- requests must be Typeable

deriving instance Eq (SleepReq a)
deriving instance Show (SleepReq a)

instance ShowP SleepReq where showp = show

instance Hashable (SleepReq a) where
   hashWithSalt s (Sleep n) = hashWithSalt s n

instance StateKey SleepReq where
  data State SleepReq = ExampleState {}

instance DataSourceName SleepReq where
  dataSourceName _ = "SleepDataSource"

instance DataSource u SleepReq where
  fetch  _state _flags _user = BackgroundFetch $ mapM_ fetch1
  schedulerHint _ = SubmitImmediately

initGlobalState :: IO (State SleepReq)
initGlobalState = return ExampleState { }

fetch1 :: BlockedFetch SleepReq -> IO ()
fetch1 (BlockedFetch (Sleep n) rvar) =
  void $ forkFinally (threadDelay (n*1000) >> return n) (putResult rvar)
