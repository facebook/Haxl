-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file. An additional grant of patent rights can
-- be found in the PATENTS file.

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

-- | An exception hierarchy that can be used with the 'Haxl' monad.
--
-- The Haxl framework may throw exceptions from this hierarchy: for
-- example, a misbehaving data source causes 'dataFetch' to throw a
-- 'DataSourceError'.  The combinator 'withDefault' from
-- "Haxl.Core.Prelude" uses this hierarchy to provide default values
-- for expressions that raise 'TransientError' or 'LogicError'
-- exceptions.
--
-- You are under no obligations to use this hierarchy for your own
-- exceptions, but you might find it useful nonetheless; for
-- 'withDefault' to be useful, for example, you'll want your
-- exceptions to be children of 'LogicError' or 'TransientError' as
-- appropriate.
--
-- Most users should import "Haxl.Core" instead of importing this
-- module directly.
module Haxl.Core.Exception (

  HaxlException(..),

  -- * Exception categories
  InternalError(..),
  internalErrorToException,
  internalErrorFromException,

  LogicError(..),
  logicErrorToException,
  logicErrorFromException,

  TransientError(..),
  transientErrorToException,
  transientErrorFromException,

  -- ** Internal exceptions
  CriticalError(..),
  DataSourceError(..),

  -- ** Logic exceptions
  NotFound(..),
  UnexpectedType(..),
  EmptyList(..),
  JSONError(..),
  InvalidParameter(..),

  -- ** Transient exceptions
  FetchError(..),

  -- * Exception utilities
  asHaxlException,

  ) where

import Control.Exception
import Data.Aeson
import Data.Typeable
import Data.Text (Text)

import Haxl.Core.Util

-- | We have a 3-tiered hierarchy of exceptions, with 'HaxlException' at
-- the top, and all Haxl exceptions as children of this. Users should
-- never deal directly with 'HaxlException's.
--
-- The main types of exceptions are:
--
--   ['InternalError']  Something is wrong with Haxl core.
--
--   ['LogicError']     Something is wrong with Haxl client code.
--
--   ['TransientError'] Something is temporarily failing (usually in a fetch).
--
-- These are not meant to be thrown (but likely be caught). Thrown
-- exceptions should be a subclass of one of these. There are some
-- generic leaf exceptions defined below this, such as 'FetchError'
-- (generic transient failure) or 'CriticalError' (internal failure).
--
data HaxlException
  = forall e. (Exception e, MiddleException e) => HaxlException e
  deriving (Typeable)

instance Show HaxlException where
  show (HaxlException e) = show e

instance Exception HaxlException

-- | These need to be serializable to JSON to cross FFI boundaries.
instance ToJSON HaxlException where
  toJSON (HaxlException e) = object
    [ "type" .= show (typeOf e)
    , "name" .= eName e
    , "txt"  .= show e
    ]

haxlExceptionToException
  :: (Exception e, MiddleException e) => e -> SomeException
haxlExceptionToException = toException . HaxlException

haxlExceptionFromException
  :: (Exception e, MiddleException e) => SomeException -> Maybe e
haxlExceptionFromException x = do
  HaxlException a <- fromException x
  cast a

class (Exception a) => MiddleException a where
  eName :: a -> String

-- | For transient failures.
data TransientError = forall e . (Exception e) => TransientError e
  deriving (Typeable)

deriving instance Show TransientError

instance Exception TransientError where
 toException   = haxlExceptionToException
 fromException = haxlExceptionFromException

instance MiddleException TransientError where
  eName (TransientError e) = show $ typeOf e

transientErrorToException :: (Exception e) => e -> SomeException
transientErrorToException = toException . TransientError

transientErrorFromException
  :: (Exception e) => SomeException -> Maybe e
transientErrorFromException x = do
  TransientError a <- fromException x
  cast a

-- | For errors in Haxl core code.
data InternalError = forall e . (Exception e) => InternalError e
  deriving (Typeable)

deriving instance Show InternalError

instance Exception InternalError where
  toException   = haxlExceptionToException
  fromException = haxlExceptionFromException

instance MiddleException InternalError where
  eName (InternalError e) = show $ typeOf e

internalErrorToException :: (Exception e) => e -> SomeException
internalErrorToException = toException . InternalError

internalErrorFromException
  :: (Exception e) => SomeException -> Maybe e
internalErrorFromException x = do
  InternalError a <- fromException x
  cast a

-- | For errors in Haxl client code.
data LogicError = forall e . (Exception e) => LogicError e
  deriving (Typeable)

deriving instance Show LogicError

instance Exception LogicError where
 toException   = haxlExceptionToException
 fromException = haxlExceptionFromException

instance MiddleException LogicError where
  eName (LogicError e) = show $ typeOf e

logicErrorToException :: (Exception e) => e -> SomeException
logicErrorToException = toException . LogicError

logicErrorFromException
  :: (Exception e) => SomeException -> Maybe e
logicErrorFromException x = do
  LogicError a <- fromException x
  cast a

------------------------------------------------------------------------
-- Leaf exceptions. You should throw these. Or make your own.
------------------------------------------------------------------------

-- | Generic \"critical\" exception. Something internal is
-- borked. Panic.
data CriticalError = CriticalError Text
  deriving (Typeable, Eq, Show)

instance Exception CriticalError where
  toException   = internalErrorToException
  fromException = internalErrorFromException

-- | Generic \"something was not found\" exception.
data NotFound = NotFound Text
  deriving (Typeable, Eq, Show)

instance Exception NotFound where
  toException = logicErrorToException
  fromException = logicErrorFromException

-- | Generic \"something had the wrong type\" exception.
data UnexpectedType = UnexpectedType Text
  deriving (Typeable, Eq, Show)

instance Exception UnexpectedType where
  toException = logicErrorToException
  fromException = logicErrorFromException

-- | Generic \"input list was empty\" exception.
data EmptyList = EmptyList Text
  deriving (Typeable, Eq, Show)

instance Exception EmptyList where
  toException = logicErrorToException
  fromException = logicErrorFromException

-- | Generic \"Incorrect assumptions about JSON data\" exception.
data JSONError = JSONError Text
  deriving (Typeable, Eq, Show)

instance Exception JSONError where
  toException = logicErrorToException
  fromException = logicErrorFromException

-- | Generic \"passing some invalid parameter\" exception.
data InvalidParameter = InvalidParameter Text
  deriving (Typeable, Eq, Show)

instance Exception InvalidParameter where
  toException = logicErrorToException
  fromException = logicErrorFromException

-- | Generic transient fetching exceptions.
data FetchError = FetchError Text
  deriving (Typeable, Eq, Show)

instance Exception FetchError where
  toException   = transientErrorToException
  fromException = transientErrorFromException

-- | A data source did something wrong
data DataSourceError = DataSourceError Text
  deriving (Typeable, Eq, Show)

instance Exception DataSourceError where
  toException   = internalErrorToException
  fromException = internalErrorFromException

-- | Converts all exceptions that are not derived from 'HaxlException'
-- into 'CriticalError', using 'show'.
asHaxlException :: SomeException -> HaxlException
asHaxlException e
  | Just haxl_exception <- fromException e = -- it's a HaxlException
     haxl_exception
  | otherwise =
     HaxlException (InternalError (CriticalError (textShow e)))
