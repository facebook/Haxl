-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
--
module Haxl.Core.Exception (

  HaxlException(..),

  -- * Exception categories
  InternalError(..),
  internalErrorToException,
  internalErrorFromException,

  LogicError(..),
  logicErrorToException,
  logicErrorFromException,

  LogicBug(..),
  logicBugToException,
  logicBugFromException,

  TransientError(..),
  transientErrorToException,
  transientErrorFromException,

  -- ** Internal exceptions
  CriticalError(..),
  DataSourceError(..),
  NonHaxlException(..),

  -- ** Logic exceptions
  NotFound(..),
  UnexpectedType(..),
  EmptyList(..),
  JSONError(..),
  InvalidParameter(..),
  MonadFail(..),

  -- ** Transient exceptions
  FetchError(..),

  -- * Exception utilities
  asHaxlException,
  MiddleException(..),
  rethrowAsyncExceptions,
  tryWithRethrow,
  ) where

#if __GLASGOW_HASKELL__ >= 808
import Prelude hiding (MonadFail)
#endif
import Control.Exception as Exception
import Data.Aeson
import Data.Binary (Binary)
import Data.Typeable
import Data.Text (Text)

import Haxl.Core.Util
import GHC.Stack

-- | We have a 3-tiered hierarchy of exceptions, with 'HaxlException' at
-- the top, and all Haxl exceptions as children of this. Users should
-- never deal directly with 'HaxlException's.
--
-- The main types of exceptions are:
--
--   ['InternalError']  Something is wrong with Haxl core.
--
--   ['LogicBug']       Something is wrong with Haxl client code.
--
--   ['LogicError']     Things that really should be return values, e.g.
--                      NotFound.
--
--   ['TransientError'] Something is temporarily failing (usually in a fetch).
--
-- These are not meant to be thrown (but likely be caught). Thrown
-- exceptions should be a subclass of one of these. There are some
-- generic leaf exceptions defined below this, such as 'FetchError'
-- (generic transient failure) or 'CriticalError' (internal failure).
--
data HaxlException
  = forall e. (MiddleException e)
    => HaxlException
         (Maybe Stack)  -- filled in with the call stack when thrown,
                        -- if PROFILING is on
         e
  deriving (Typeable)

type Stack = [String]
  -- hopefully this will get more informative in the future

instance Show HaxlException where
  show (HaxlException (Just stk@(_:_)) e) = show e ++ '\n' : renderStack stk
  show (HaxlException _ e) = show e

instance Exception HaxlException

-- | These need to be serializable to JSON to cross FFI boundaries.
instance ToJSON HaxlException where
  toJSON (HaxlException stk e) = object fields
    where
      fields | Just s@(_:_) <- stk = ("stack" .= reverse s) : rest
             | otherwise = rest
      rest =
        [ "type" .= show (typeOf e)
        , "name" .= eName e
        , "txt"  .= show e
        ]

haxlExceptionToException
  :: (MiddleException e) => e -> SomeException
haxlExceptionToException = toException . HaxlException Nothing

haxlExceptionFromException
  :: (MiddleException e) => SomeException -> Maybe e
haxlExceptionFromException x = do
  HaxlException _ a <- fromException x
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

data LogicBug = forall e . (Exception e) => LogicBug e
  deriving (Typeable)

deriving instance Show LogicBug

instance Exception LogicBug where
 toException   = haxlExceptionToException
 fromException = haxlExceptionFromException

instance MiddleException LogicBug where
  eName (LogicBug e) = show $ typeOf e

logicBugToException :: (Exception e) => e -> SomeException
logicBugToException = toException . LogicBug

logicBugFromException
  :: (Exception e) => SomeException -> Maybe e
logicBugFromException x = do
  LogicBug a <- fromException x
  cast a

------------------------------------------------------------------------
-- Leaf exceptions. You should throw these. Or make your own.
------------------------------------------------------------------------

-- | Generic \"critical\" exception. Something internal is
-- borked. Panic.
newtype CriticalError = CriticalError Text
  deriving (Typeable, Binary, Eq, Show)

instance Exception CriticalError where
  toException   = internalErrorToException
  fromException = internalErrorFromException

-- | Exceptions that are converted to HaxlException by
-- asHaxlException.  Typically these will be pure exceptions,
-- e.g., the 'error' function in pure code, or a pattern-match
-- failure.
newtype NonHaxlException = NonHaxlException Text
  deriving (Typeable, Binary, Eq, Show)

instance Exception NonHaxlException where
  toException   = internalErrorToException
  fromException = internalErrorFromException

-- | Generic \"something was not found\" exception.
newtype NotFound = NotFound Text
  deriving (Typeable, Binary, Eq, Show)

instance Exception NotFound where
  toException = logicErrorToException
  fromException = logicErrorFromException

-- | Generic \"something had the wrong type\" exception.
newtype UnexpectedType = UnexpectedType Text
  deriving (Typeable, Eq, Show)

instance Exception UnexpectedType where
  toException = logicErrorToException
  fromException = logicErrorFromException

-- | Generic \"input list was empty\" exception.
newtype EmptyList = EmptyList Text
  deriving (Typeable, Eq, Show)

instance Exception EmptyList where
  toException = logicErrorToException
  fromException = logicErrorFromException
  -- TODO: should be a child of LogicBug

-- | Generic \"Incorrect assumptions about JSON data\" exception.
newtype JSONError = JSONError Text
  deriving (Typeable, Eq, Show)

instance Exception JSONError where
  toException = logicErrorToException
  fromException = logicErrorFromException

-- | Generic \"passing some invalid parameter\" exception.
newtype InvalidParameter = InvalidParameter Text
  deriving (Typeable, Eq, Show)

instance Exception InvalidParameter where
  toException = logicErrorToException
  fromException = logicErrorFromException
  -- TODO: should be a child of LogicBug

-- | Generic \"fail was called\" exception.
newtype MonadFail = MonadFail Text
  deriving (Typeable, Eq, Show)

instance Exception MonadFail where
  toException = logicErrorToException
  fromException = logicErrorFromException

-- | Generic transient fetching exceptions.
newtype FetchError = FetchError Text
  deriving (Typeable, Eq, Show)

instance Exception FetchError where
  toException   = transientErrorToException
  fromException = transientErrorFromException

-- | A data source did something wrong
newtype DataSourceError = DataSourceError Text
  deriving (Typeable, Eq, Show)

instance Exception DataSourceError where
  toException   = internalErrorToException
  fromException = internalErrorFromException

-- | Converts all exceptions that are not derived from 'HaxlException'
-- into 'NonHaxlException', using 'show'.
asHaxlException :: SomeException -> HaxlException
asHaxlException e
  | Just haxl_exception <- fromException e = -- it's a HaxlException
     haxl_exception
  | otherwise =
     HaxlException Nothing (InternalError (NonHaxlException (textShow e)))

-- We must be careful about turning IO monad exceptions into Haxl
-- exceptions.  An IO monad exception will normally propagate right
-- out of runHaxl and terminate the whole computation, whereas a Haxl
-- exception can get dropped on the floor, if it is on the right of
-- <*> and the left side also throws, for example.  So turning an IO
-- monad exception into a Haxl exception is a dangerous thing to do.
-- In particular, we never want to do it for an asynchronous exception
-- (AllocationLimitExceeded, ThreadKilled, etc.), because these are
-- supposed to unconditionally terminate the computation.
--
-- There are three places where we take an arbitrary IO monad exception and
-- turn it into a Haxl exception:
--
--  * wrapFetchInCatch.  Here we want to propagate a failure of the
--    data source to the callers of the data source, but if the
--    failure came from elsewhere (an asynchronous exception), then we
--    should just propagate it
--
--  * cacheResult (cache the results of IO operations): again,
--    failures of the IO operation should be visible to the caller as
--    a Haxl exception, but we exclude asynchronous exceptions from
--    this.

--  * unsafeToHaxlException: assume the caller knows what they're
--    doing, and just wrap all exceptions.
--
rethrowAsyncExceptions :: SomeException -> IO ()
rethrowAsyncExceptions e
  | Just SomeAsyncException{} <- fromException e = Exception.throw e
  | otherwise = return ()

tryWithRethrow :: IO a -> IO (Either SomeException a)
tryWithRethrow io =
  (Right <$> io) `catch` \e -> do rethrowAsyncExceptions e ; return (Left e)
