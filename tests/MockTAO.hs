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
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module MockTAO (
    Id(..),
    initGlobalState,
    assocRangeId2s,
    friendsAssoc,
    friendsOf,
  ) where

import Data.Hashable
import Data.Map (Map)
import Data.Typeable
import Prelude ()
import qualified Data.Map as Map
import qualified Data.Text as Text

import Haxl.Prelude
import Haxl.Core

import TestTypes

-- -----------------------------------------------------------------------------
-- Minimal mock TAO

data TAOReq a where
  AssocRangeId2s :: Id -> Id -> TAOReq [Id]
  deriving Typeable

deriving instance Show (TAOReq a)
deriving instance Eq (TAOReq a)

instance ShowP TAOReq where showp = show

instance Hashable (TAOReq a) where
  hashWithSalt s (AssocRangeId2s a b) = hashWithSalt s (a,b)

instance StateKey TAOReq where
  data State TAOReq = TAOState { future :: Bool }

instance DataSourceName TAOReq where
  dataSourceName _ = "MockTAO"

instance DataSource UserEnv TAOReq where
  fetch TAOState{..} _flags _user
    | future = FutureFetch $ return . mapM_ doFetch
    | otherwise = SyncFetch $ mapM_ doFetch

initGlobalState :: Bool -> IO (State TAOReq)
initGlobalState future = return TAOState { future=future }

doFetch :: BlockedFetch TAOReq -> IO ()
doFetch (BlockedFetch req@(AssocRangeId2s a b) r) =
  case Map.lookup (a, b) assocs of
    Nothing -> putFailure r . NotFound . Text.pack $ show req
    Just result -> putSuccess r result

assocs :: Map (Id,Id) [Id]
assocs = Map.fromList [
  ((friendsAssoc, 1), [5..10]),
  ((friendsAssoc, 2), [7..12]),
  ((friendsAssoc, 3), [10..15]),
  ((friendsAssoc, 4), [15..19])
 ]

friendsAssoc :: Id
friendsAssoc = 167367433327742

assocRangeId2s :: Id -> Id -> Haxl [Id]
assocRangeId2s a b = dataFetch (AssocRangeId2s a b)

friendsOf :: Id -> Haxl [Id]
friendsOf = assocRangeId2s friendsAssoc
