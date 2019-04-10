-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module SleepDataSource (
    sleep,
  ) where

import Haxl.Prelude
import Prelude ()

import Haxl.Core
import Haxl.DataSource.ConcurrentIO

import Control.Concurrent
import Data.Hashable
import Data.Typeable

sleep :: Int -> GenHaxl u w Int
sleep n = dataFetch (Sleep n)

data Sleep deriving Typeable
instance ConcurrentIO Sleep where
  data ConcurrentIOReq Sleep a where
    Sleep :: Int -> ConcurrentIOReq Sleep Int

  performIO (Sleep n) = threadDelay (n*1000) >> return n

deriving instance Eq (ConcurrentIOReq Sleep a)
deriving instance Show (ConcurrentIOReq Sleep a)

instance ShowP (ConcurrentIOReq Sleep) where showp = show

instance Hashable (ConcurrentIOReq Sleep a) where
  hashWithSalt s (Sleep n) = hashWithSalt s n
