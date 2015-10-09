-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file. An additional grant of patent rights can
-- be found in the PATENTS file.

{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | A cache mapping data requests to their results.  This module is
-- provided for access to Haxl internals only; most users should not
-- need to import it.
module Haxl.Core.DataCache
  ( DataCache
  , empty
  , insert
  , insertNotShowable
  , lookup
  , showCache
  ) where

import Data.HashMap.Strict (HashMap)
import Data.Hashable
import Prelude hiding (lookup)
import Unsafe.Coerce
import qualified Data.HashMap.Strict as HashMap
import Data.Typeable.Internal
import Data.Maybe
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative hiding (empty)
#endif
import Control.Exception

import Haxl.Core.Types

-- | The 'DataCache' maps things of type @f a@ to @'ResultVar' a@, for
-- any @f@ and @a@ provided @f a@ is an instance of 'Typeable'. In
-- practice @f a@ will be a request type parameterised by its result.
--
-- See the definition of 'ResultVar' for more details.

newtype DataCache res = DataCache (HashMap TypeRep (SubCache res))

-- | The implementation is a two-level map: the outer level maps the
-- types of requests to 'SubCache', which maps actual requests to their
-- results.  So each 'SubCache' contains requests of the same type.
-- This works well because we only have to store the dictionaries for
-- 'Hashable' and 'Eq' once per request type.
data SubCache res =
  forall req a . (Hashable (req a), Eq (req a)) =>
       SubCache (req a -> String) (a -> String) ! (HashMap (req a) (res a))
       -- NB. the inner HashMap is strict, to avoid building up
       -- a chain of thunks during repeated insertions.

-- | A new, empty 'DataCache'.
empty :: DataCache res
empty = DataCache HashMap.empty

-- | Inserts a request-result pair into the 'DataCache'.
insert
  :: (Hashable (req a), Typeable (req a), Eq (req a), Show (req a), Show a)
  => req a
  -- ^ Request
  -> res a
  -- ^ Result
  -> DataCache res
  -> DataCache res

insert req result (DataCache m) =
  DataCache $
    HashMap.insertWith fn (typeOf req)
       (SubCache show show (HashMap.singleton req result)) m
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

insertNotShowable req result (DataCache m) =
  DataCache $
    HashMap.insertWith fn (typeOf req)
       (SubCache notShowable notShowable (HashMap.singleton req result)) m
  where
    fn (SubCache _ _ new) (SubCache showReq showRes old) =
      SubCache showReq showRes (unsafeCoerce new `HashMap.union` old)

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
-- converted to 'String's using 'show'.  The entries are grouped by
-- 'TypeRep'.  Note that this will fail if 'insertNotShowable' has
-- been used to insert any entries.
--
showCache
  :: DataCache ResultVar
  -> IO [(TypeRep, [(String, Either SomeException String)])]

showCache (DataCache cache) = mapM goSubCache (HashMap.toList cache)
 where
  goSubCache
    :: (TypeRep,SubCache ResultVar)
    -> IO (TypeRep,[(String, Either SomeException String)])
  goSubCache (ty, SubCache showReq showRes hmap) = do
    elems <- catMaybes <$> mapM go (HashMap.toList hmap)
    return (ty, elems)
   where
    go  (req, rvar) = do
      maybe_r <- tryReadResult rvar
      case maybe_r of
        Nothing -> return Nothing
        Just (Left e) -> return (Just (showReq req, Left e))
        Just (Right result) ->
          return (Just (showReq req, Right (showRes result)))
