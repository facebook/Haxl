{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE RankNTypes, GADTs, BangPatterns, DeriveDataTypeable,
    StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind -fno-warn-type-defaults #-}

module Bench where

import Haxl.Core.DataCache as DataCache

import Prelude hiding (mapM)

import Data.Hashable
import Data.IORef
import Data.Time.Clock
import Data.Traversable
import Data.Typeable
import System.Environment
import Text.Printf

data TestReq a where
  ReqInt    :: {-# UNPACK #-} !Int -> TestReq Int
  ReqDouble :: {-# UNPACK #-} !Int -> TestReq Double
  ReqBool   :: {-# UNPACK #-} !Int -> TestReq Bool
  deriving Typeable

deriving instance Eq (TestReq a)
deriving instance Show (TestReq a)

instance Hashable (TestReq a) where
  hashWithSalt salt (ReqInt i) = hashWithSalt salt (0::Int, i)
  hashWithSalt salt (ReqDouble i) = hashWithSalt salt (1::Int, i)
  hashWithSalt salt (ReqBool i) = hashWithSalt salt (2::Int, i)

main = do
  [n] <- fmap (fmap read) getArgs
  t0 <- getCurrentTime
  cache <- emptyDataCache
  let
     f 0 = return ()
     f !n = do
       m <- newIORef 0
       DataCache.insert (ReqInt n) m cache
       f (n-1)
  --
  f n
  m <- DataCache.lookup (ReqInt (n `div` 2)) cache
  print =<< mapM readIORef m
  t1 <- getCurrentTime
  printf "insert: %.2fs\n" (realToFrac (t1 `diffUTCTime` t0) :: Double)

  t0 <- getCurrentTime
  let
     f 0  !m = return m
     f !n !m = do
      mbRes <- DataCache.lookup (ReqInt n) cache
      case mbRes of
        Nothing -> f (n-1) m
        Just _  -> f (n-1) (m+1)
  f n 0 >>= print
  t1 <- getCurrentTime
  printf "lookup: %.2fs\n" (realToFrac (t1 `diffUTCTime` t0) :: Double)
