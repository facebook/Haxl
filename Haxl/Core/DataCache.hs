-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- A cache mapping data requests to their results.  This module is
-- provided for access to Haxl internals only; most users should not
-- need to import it.
--
module Haxl.Core.DataCache
  ( DataCache(..)
  , SubCache(..)
  , emptyDataCache
  , filter
  , insert
  , insertNotShowable
  , insertWithShow
  , lookup
  , showCache
  , readCache
  ) where

import Prelude hiding (lookup, filter)
import Control.Exception
import Unsafe.Coerce
import Data.Typeable
import Data.Hashable
import qualified Data.HashTable.IO as H

-- ---------------------------------------------------------------------------
-- DataCache

-- | A @'DataCache' res@ maps things of type @req a@ to @res a@, for
-- any @req@ and @a@ provided @req a@ is an instance of 'Typeable'. In
-- practice @req a@ will be a request type parameterised by its result.
--
newtype DataCache res = DataCache (HashTable TypeRep (SubCache res))

-- | The implementation is a two-level map: the outer level maps the
-- types of requests to 'SubCache', which maps actual requests to their
-- results.  So each 'SubCache' contains requests of the same type.
-- This works well because we only have to store the dictionaries for
-- 'Hashable' and 'Eq' once per request type.
--
data SubCache res =
  forall req a . (Hashable (req a), Eq (req a), Typeable (req a)) =>
       SubCache (req a -> String) (a -> String) ! (HashTable (req a) (res a))
       -- NB. the inner HashMap is strict, to avoid building up
       -- a chain of thunks during repeated insertions.

type HashTable k v = H.BasicHashTable k v

-- | A new, empty 'DataCache'.
emptyDataCache :: IO (DataCache res)
emptyDataCache = DataCache <$> H.new

-- | Inserts a request-result pair into the 'DataCache'.
insert
  :: (Hashable (req a), Typeable (req a), Eq (req a), Show (req a), Show a)
  => req a
  -- ^ Request
  -> res a
  -- ^ Result
  -> DataCache res
  -> IO ()

insert = insertWithShow show show

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
  -> IO ()

insertNotShowable = insertWithShow notShowable notShowable

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
  -> IO ()

insertWithShow showRequest showResult request result (DataCache m) =
  H.mutateIO m (typeOf request) (mutate showRequest showResult request result)

notShowable :: a
notShowable = error "insertNotShowable"

-- | A mutation function for mutateIO. If the key doesn't exist in the top-level
-- cache, creates a new hashtable and inserts the request and result.
-- If the key exists, insert the request and result into the existing subcache,
-- replacing any existing mapping.
mutate :: (Hashable (req a), Typeable (req a), Eq (req a))
  => (req a -> String)
  -> (a -> String)
  -> req a
  -> res a
  -> Maybe (SubCache res)
  -> IO (Maybe (SubCache res), ())
mutate showRequest showResult request result Nothing = do
  newTable <- H.new
  H.insert newTable request result
  return (Just (SubCache showRequest showResult newTable), ())
mutate _ _ request result (Just sc@(SubCache _ _ oldTable)) = do
    H.insert oldTable (unsafeCoerce request) (unsafeCoerce result)
    return (Just sc, ())

-- | Looks up the cached result of a request.
lookup
  :: Typeable (req a)
  => req a
  -- ^ Request
  -> DataCache res
  -> IO (Maybe (res a))

lookup req (DataCache m) = do
  mbRes <- H.lookup m (typeOf req)
  case mbRes of
    Nothing -> return Nothing
    Just (SubCache _ _ sc) ->
      unsafeCoerce (H.lookup sc (unsafeCoerce req))

filter
  :: forall res
  . (forall a. res a -> IO Bool)
  -> DataCache res
  -> IO (DataCache res)
filter pred (DataCache cache) = do
  cacheList <- H.toList cache
  filteredCache <- filterSubCache `mapM` cacheList
  DataCache <$> H.fromList filteredCache
  where
    filterSubCache
      :: (TypeRep, SubCache res)
      -> IO (TypeRep, SubCache res)
    filterSubCache (ty, SubCache showReq showRes hm) = do
      filteredList <- H.foldM go [] hm
      filteredSC <- H.fromList filteredList
      return (ty, SubCache showReq showRes filteredSC)
      where
        go res (request, rvar) = do
          predRes <- pred rvar
          return $ if predRes then (request, rvar):res else res

-- | Dumps the contents of the cache, with requests and responses
-- converted to 'String's using the supplied show functions.  The
-- entries are grouped by 'TypeRep'.  Note that this will fail if
-- 'insertNotShowable' has been used to insert any entries.
showCache
  :: forall res
  .  DataCache res
  -> (forall a . res a -> IO (Maybe (Either SomeException a)))
  -> IO [(TypeRep, [(String, Either SomeException String)])]
showCache (DataCache cache) readRes = H.foldM goSubCache [] cache
  where
    goSubCache
      :: [(TypeRep, [(String, Either SomeException String)])]
      -> (TypeRep, SubCache res)
      -> IO [(TypeRep, [(String, Either SomeException String)])]
    goSubCache res (ty, SubCache showReq showRes hm) = do
      subCacheResult <- H.foldM go [] hm
      return $ (ty, subCacheResult):res
      where
        go res (request, rvar) = do
          maybe_r <- readRes rvar
          return $ case maybe_r of
            Nothing -> res
            Just (Left e) -> (showReq request, Left e) : res
            Just (Right result) ->
              (showReq request, Right (showRes result)) : res

-- | Dumps the contents of the cache responses to list
readCache
  :: forall res ret
  .  DataCache res
  -> (forall a . res a -> IO ret)
  -> IO [(TypeRep, [Either SomeException ret])]
readCache (DataCache cache) readRes = H.foldM goSubCache [] cache
  where
    goSubCache
      :: [(TypeRep, [Either SomeException ret])]
      -> (TypeRep, SubCache res)
      -> IO [(TypeRep, [Either SomeException ret])]
    goSubCache res (ty, SubCache _showReq _showRes hm) = do
      subCacheResult <- H.foldM go [] hm
      return $ (ty, subCacheResult):res
      where
        go res (_request, rvar) = do
          r <- try $ readRes rvar
          return $ r : res
