{-# LANGUAGE RankNTypes, GADTs, BangPatterns, DeriveDataTypeable,
    StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind -fno-warn-type-defaults #-}

module Bench where

import Haxl.Core.DataCache as DataCache
import Haxl.Core.Types

import Prelude hiding (mapM)

import Data.Hashable
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
  let
     f 0  !cache = return cache
     f !n !cache = do
       m <- newResult 0
       f (n-1) (DataCache.insert (ReqInt n) m cache)
  --
  cache <- f n DataCache.empty
  let m = DataCache.lookup (ReqInt (n `div` 2)) cache
  print =<< mapM takeResult m
  t1 <- getCurrentTime
  printf "insert: %.2fs\n" (realToFrac (t1 `diffUTCTime` t0) :: Double)

  t0 <- getCurrentTime
  let
     f 0  !m = return m
     f !n !m = case DataCache.lookup (ReqInt n) cache of
                 Nothing -> f (n-1) m
                 Just _  -> f (n-1) (m+1)
  f n 0 >>= print
  t1 <- getCurrentTime
  printf "lookup: %.2fs\n" (realToFrac (t1 `diffUTCTime` t0) :: Double)
