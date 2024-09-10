{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module WorkDataSource (
    mkWorkState,
    work,
  ) where

import Haxl.Prelude
import Prelude ()

import Haxl.Core

import Control.Exception
import Data.Hashable
import Data.Typeable
import Control.Monad (void)
import Control.Concurrent.MVar


data Work a where
  Work :: Integer -> Work Integer
  deriving Typeable

deriving instance Eq (Work a)
deriving instance Show (Work a)
instance ShowP Work where showp = show

instance Hashable (Work a) where
   hashWithSalt s (Work a) = hashWithSalt s (0::Int,a)

instance DataSourceName Work where
  dataSourceName _ = "Work"

instance StateKey Work where
  data State Work = WorkState

newtype Service = Service (MVar [IO ()])

run :: Work a -> IO a
run (Work n) = evaluate (sum [1..n]) >> return n

mkService :: IO Service
mkService = Service <$> newMVar []

process :: Service -> IO ()
process (Service q) = do
  r <- swapMVar q []
  sequence_ r

enqueue :: Service -> Work a -> IO (IO (Either SomeException a))
enqueue (Service q) w = do
  res <- newEmptyMVar
  let r = do
        v <- Control.Exception.try $ run w
        putMVar res v
  modifyMVar_ q (return . (:) r)
  return (takeMVar res)

instance DataSource u Work where
  fetch = backgroundFetchAcquireReleaseMVar
    mkService
    (\_ -> return ())
    -- pretend we are ready so that process does the work
    (\_ _ m -> void $ tryPutMVar m ())
    process
    enqueue


mkWorkState :: State Work
mkWorkState = WorkState

work :: Integer -> GenHaxl u w Integer
work n = dataFetch (Work n)
