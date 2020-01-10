-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- A cache mapping data requests to their results.  This module is
-- provided for access to Haxl internals only; most users should not
-- need to import it.
--
module Haxl.Core.DataCache
  ( DataCache(..)
  , SubCache(..)
  , emptyDataCache
  , insert
  , insertNotShowable
  , insertWithShow
  , lookup
  , showCache
  ) where

import Control.Exception
import Data.Hashable
import Prelude hiding (lookup)
import Unsafe.Coerce
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Typeable
import Data.Maybe

-- ---------------------------------------------------------------------------
-- DataCache

-- | A @'DataCache' res@ maps things of type @req a@ to @res a@, for
-- any @req@ and @a@ provided @req a@ is an instance of 'Typeable'. In
-- practice @req a@ will be a request type parameterised by its result.
--
newtype DataCache res = DataCache (HashMap TypeRep (SubCache res))

-- | The implementation is a two-level map: the outer level maps the
-- types of requests to 'SubCache', which maps actual requests to their
-- results.  So each 'SubCache' contains requests of the same type.
-- This works well because we only have to store the dictionaries for
-- 'Hashable' and 'Eq' once per request type.
--
data SubCache res =
  forall req a . (Hashable (req a), Eq (req a), Typeable (req a)) =>
       SubCache (req a -> String) (a -> String) ! (HashMap (req a) (res a))
       -- NB. the inner HashMap is strict, to avoid building up
       -- a chain of thunks during repeated insertions.

-- | A new, empty 'DataCache'.
emptyDataCache :: DataCache res
emptyDataCache = DataCache HashMap.empty


-- | Inserts a request-result pair into the 'DataCache'.
insert
  :: (Hashable (req a), Typeable (req a), Eq (req a), Show (req a), Show a)
  => req a
  -- ^ Request
  -> res a
  -- ^ Result
  -> DataCache res
  -> DataCache res

insert = insertWithShow show show

-- | Inserts a request-result pair into the 'DataCache', with the given
-- functions used to show the request and result.
insertWithShow
  :: (Hashable (req a), Typeable (req a), Eq (req a))
  => (req a -> String)
  -- ^ Show function for request
  -> (a -> String)
  -- ^ Show function for result
  -> req a
  -- ^ Request
  -> res a
  -- ^ Result
  -> DataCache res
  -> DataCache res

insertWithShow showRequest showResult req result (DataCache m) =
  DataCache $
    HashMap.insertWith fn (typeOf req)
       (SubCache showRequest showResult (HashMap.singleton req result)) m
  where
    fn (SubCache _ _ new) (SubCache showReq showRes old) =
      SubCache showReq showRes (unsafeCoerce new `HashMap.union` old)

-- | Inserts a request-result pair into the 'DataCache', without
-- requiring Show instances of the request or the result.  The cache
-- cannot be subsequently used with `showCache`.
insertNotShowable
  :: (Hashable (req a), Typeable (req a), Eq (req a))
  => req a
  -- ^ Request
  -> res a
  -- ^ Result
  -> DataCache res
  -> DataCache res

insertNotShowable = insertWithShow notShowable notShowable

notShowable :: a
notShowable = error "insertNotShowable"

-- | Looks up the cached result of a request.
lookup
  :: Typeable (req a)
  => req a
  -- ^ Request
  -> DataCache res
  -> Maybe (res a)

lookup req (DataCache m) =
      case HashMap.lookup (typeOf req) m of
        Nothing -> Nothing
        Just (SubCache _ _ sc) ->
           unsafeCoerce (HashMap.lookup (unsafeCoerce req) sc)

-- | Dumps the contents of the cache, with requests and responses
-- converted to 'String's using the supplied show functions.  The
-- entries are grouped by 'TypeRep'.  Note that this will fail if
-- 'insertNotShowable' has been used to insert any entries.
--
showCache
  :: forall res
  .  DataCache res
  -> (forall a . res a -> IO (Maybe (Either SomeException a)))
  -> IO [(TypeRep, [(String, Either SomeException String)])]

showCache (DataCache cache) readRes = mapM goSubCache (HashMap.toList cache)
 where
  goSubCache
    :: (TypeRep, SubCache res)
    -> IO (TypeRep,[(String, Either SomeException String)])
  goSubCache (ty, SubCache showReq showRes hmap) = do
    elems <- catMaybes <$> mapM go (HashMap.toList hmap)
    return (ty, elems)
   where
    go  (req, rvar) = do
      maybe_r <- readRes rvar
      case maybe_r of
        Nothing -> return Nothing
        Just (Left e) -> return (Just (showReq req, Left e))
        Just (Right result) ->
          return (Just (showReq req, Right (showRes result)))
