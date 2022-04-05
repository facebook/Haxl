-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}

module TestTypes
   ( UserEnv
   , Haxl
   , HaxlEnv
   , lookupInput
   , Id(..)
   ) where

import Data.Aeson
import Data.Binary (Binary)
import qualified Data.Text as Text
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Key (toText)
#else
import qualified Data.HashMap.Strict as KeyMap
#endif
import Data.Hashable
import Data.Typeable

import Haxl.Core

#if !MIN_VERSION_aeson(2,0,0)
type Key = Text.Text

toText :: Key -> Text.Text
toText = id
#endif

type UserEnv = Object
type Haxl a = GenHaxl UserEnv () a
type HaxlEnv = Env UserEnv ()

lookupInput :: FromJSON a => Key -> Haxl a
lookupInput field = do
  mb_val <- env (KeyMap.lookup field . userEnv)
  case mb_val of
    Nothing ->
      throw (NotFound (Text.concat ["field ", toText field, " was not found."]))
    Just val ->
      case fromJSON val of
        Error str ->
          throw (UnexpectedType (Text.concat
            ["field ", toText field, ": ", Text.pack str]))
        Success a -> return a


newtype Id = Id Int
  deriving (Eq, Ord, Binary, Enum, Num, Integral, Real, Hashable, Typeable,
            ToJSON, FromJSON)

instance Show Id where
  show (Id i) = show i
