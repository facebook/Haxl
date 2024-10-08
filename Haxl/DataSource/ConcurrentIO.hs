{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- A generic Haxl datasource for performing arbitrary IO concurrently.
-- Every IO operation will be performed in a separate thread.
-- You can use this with any kind of IO, but each different operation
-- requires an instance of the 'ConcurrentIO' class.
--
-- For example, to make a concurrent sleep operation:
--
-- > sleep :: Int -> GenHaxl u w Int
-- > sleep n = dataFetch (Sleep n)
-- >
-- > data Sleep
-- > instance ConcurrentIO Sleep where
-- >   data ConcurrentIOReq Sleep a where
-- >     Sleep :: Int -> ConcurrentIOReq Sleep Int
-- >
-- >   performIO (Sleep n) = threadDelay (n*1000) >> return n
-- >
-- > deriving instance Eq (ConcurrentIOReq Sleep a)
-- > deriving instance Show (ConcurrentIOReq Sleep a)
-- >
-- > instance ShowP (ConcurrentIOReq Sleep) where showp = show
-- >
-- > instance Hashable (ConcurrentIOReq Sleep a) where
-- >   hashWithSalt s (Sleep n) = hashWithSalt s n
--
-- Note that you can have any number of constructors in your
-- ConcurrentIOReq GADT, so most of the boilerplate only needs to be
-- written once.

module Haxl.DataSource.ConcurrentIO
  ( mkConcurrentIOState
  , ConcurrentIO(..)
  ) where

import Control.Concurrent
import Control.Monad
import qualified Data.Text as Text
import Data.Typeable

import Haxl.Core

class ConcurrentIO tag where
  data ConcurrentIOReq tag a
  performIO :: ConcurrentIOReq tag a -> IO a

deriving instance Typeable ConcurrentIOReq -- not needed by GHC 7.10 and later

instance (Typeable tag) => StateKey (ConcurrentIOReq tag) where
  data State (ConcurrentIOReq tag) = ConcurrentIOState
  getStateType _ = typeRep (Proxy :: Proxy ConcurrentIOReq)

mkConcurrentIOState :: IO (State (ConcurrentIOReq ()))
mkConcurrentIOState = return ConcurrentIOState

instance Typeable tag => DataSourceName (ConcurrentIOReq tag) where
  dataSourceName _ =
    Text.pack (show (typeRepTyCon (typeRep (Proxy :: Proxy tag))))

instance
  (Typeable tag, ShowP (ConcurrentIOReq tag), ConcurrentIO tag)
  => DataSource u (ConcurrentIOReq tag)
 where
  fetch _state _flags _u = BackgroundFetch $ \bfs -> do
    forM_ bfs $ \(BlockedFetch req rv) ->
      forkFinally (performIO req) (putResultFromChildThread rv)
