{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module TestTypes
   ( UserEnv
   , Haxl
   , lookupInput
   , Id(..)
   ) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable
import Data.Typeable

import Haxl.Core

type UserEnv = Object
type Haxl a = GenHaxl UserEnv a

lookupInput :: FromJSON a => Text -> Haxl a
lookupInput field = do
  mb_val <- env (HashMap.lookup field . userEnv)
  case mb_val of
    Nothing ->
      throw (NotFound (Text.concat ["field ", field, " was not found."]))
    Just val ->
      case fromJSON val of
        Error str ->
          throw (UnexpectedType (Text.concat
            ["field ", field, ": ", Text.pack str]))
        Success a -> return a


newtype Id = Id Int
  deriving (Eq, Ord, Enum, Num, Integral, Real, Hashable, Typeable,
            ToJSON, FromJSON)

instance Show Id where
  show (Id i) = show i
