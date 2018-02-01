-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module WorkDataSource (
    work,
  ) where

import Haxl.Prelude
import Prelude ()

import Haxl.Core
import Haxl.DataSource.ConcurrentIO

import Control.Exception
import Data.Hashable
import Data.Typeable

work :: Integer -> GenHaxl u Integer
work n = dataFetch (Work n)

data Work deriving Typeable
instance ConcurrentIO Work where
  data ConcurrentIOReq Work a where
    Work :: Integer -> ConcurrentIOReq Work Integer

  performIO (Work n) = evaluate (sum [1..n]) >> return n

deriving instance Eq (ConcurrentIOReq Work a)
deriving instance Show (ConcurrentIOReq Work a)

instance ShowP (ConcurrentIOReq Work) where showp = show

instance Hashable (ConcurrentIOReq Work a) where
  hashWithSalt s (Work n) = hashWithSalt s n
