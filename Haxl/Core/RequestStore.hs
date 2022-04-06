-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BangPatterns #-}
-- | Bucketing requests by 'DataSource'.
--
-- When a request is issued by the client via 'dataFetch', it is placed
-- in the 'RequestStore'. When we are ready to fetch the current batch
-- of requests, the 'contents' operation extracts the fetches, bucketed
-- by 'DataSource'.
--
-- This module is provided for access to Haxl internals only; most
-- users should not need to import it.
--
module Haxl.Core.RequestStore
  ( BlockedFetches(..)
  , BlockedFetchInternal(..)
  , RequestStore
  , isEmpty
  , noRequests
  , addRequest
  , contents
  , getSize
  , ReqCountMap(..)
  , emptyReqCounts
  , filterRCMap
  , getMapFromRCMap
  , getSummaryMapFromRCMap
  , addToCountMap
  , subFromCountMap
  ) where

import Haxl.Core.DataSource
import Haxl.Core.Stats
import Data.Map (Map)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import Data.Proxy
import Data.Text (Text)
import Data.Kind (Type)
import Data.Typeable
import Unsafe.Coerce

-- | A container for multiple 'BlockedFetch' objects.
newtype RequestStore u = RequestStore (Map TypeRep (BlockedFetches u))
  -- Since we don't know which data sources we will be using, the store
  -- is dynamically-typed.  It maps the TypeRep of the request to the
  -- 'BlockedFetches' for that 'DataSource'.

newtype BlockedFetchInternal = BlockedFetchInternal CallId

-- | A batch of 'BlockedFetch' objects for a single 'DataSource'
data BlockedFetches u =
  forall r. (DataSource u r) =>
        BlockedFetches [BlockedFetch r] [BlockedFetchInternal]

isEmpty :: RequestStore u -> Bool
isEmpty (RequestStore m) = Map.null m

-- | A new empty 'RequestStore'.
noRequests :: RequestStore u
noRequests = RequestStore Map.empty

-- | Adds a 'BlockedFetch' to a 'RequestStore'.
addRequest
  :: forall u r. (DataSource u r)
  => BlockedFetch r -> BlockedFetchInternal -> RequestStore u -> RequestStore u
addRequest bf bfi (RequestStore m) =
  RequestStore $ Map.insertWith combine ty (BlockedFetches [bf] [bfi]) m
 where
  combine :: BlockedFetches u -> BlockedFetches u -> BlockedFetches u
  combine _ (BlockedFetches bfs bfis)
    | typeOf1 (getR bfs) == ty = BlockedFetches (unsafeCoerce bf:bfs) (bfi:bfis)
    | otherwise                = error "RequestStore.insert"
         -- the dynamic type check here should be unnecessary, but if
         -- there are bugs in `Typeable` or `Map` then we'll get an
         -- error instead of a crash.  The overhead is negligible.

  -- a type conversion only, so we can get the type of the reqeusts from
  -- the list of BlockedFetch.
  getR :: [BlockedFetch r1] -> r1 a
  getR _ = undefined

  -- The TypeRep of requests for this data source
  ty :: TypeRep
  !ty = typeOf1 (undefined :: r a)

-- | Retrieves the whole contents of the 'RequestStore'.
contents :: RequestStore u -> [BlockedFetches u]
contents (RequestStore m) = Map.elems m

getSize :: RequestStore u -> Int
getSize (RequestStore m) = Map.size m

-- A counter to keep track of outgone requests. Entries are added to this
-- map as we send requests to datasources, and removed as these fetches
-- are completed.
-- This is a 2 level map: the 1st level stores requests for a particular
-- datasource, the 2nd level stores count of requests per type.
newtype ReqCountMap = ReqCountMap (Map Text (Map TypeRep Int))
  deriving (Show)

emptyReqCounts :: ReqCountMap
emptyReqCounts = ReqCountMap Map.empty

addToCountMap
  :: forall (r :: Type -> Type). (DataSourceName r, Typeable r)
  => Proxy r
  -> Int -- type and number of requests
  -> ReqCountMap
  -> ReqCountMap
addToCountMap = updateCountMap (+)

subFromCountMap
  :: forall (r :: Type -> Type). (DataSourceName r, Typeable r)
  => Proxy r
  -> Int -- type and number of requests
  -> ReqCountMap
  -> ReqCountMap
subFromCountMap = updateCountMap (-)

updateCountMap
  :: forall (r :: Type -> Type). (DataSourceName r, Typeable r)
  => (Int -> Int -> Int)
  -> Proxy r
  -> Int -- type and number of requests
  -> ReqCountMap
  -> ReqCountMap
updateCountMap op p n (ReqCountMap m) = ReqCountMap $ Map.insertWith
  (flip (Map.unionWith op)) -- flip is important as "op" is not commutative
  (dataSourceName p) (Map.singleton ty n)
  m
  where
    -- The TypeRep of requests for this data source
    -- The way this is implemented, all elements in the 2nd level map will be
    -- mapped to the same key, as all requests to a datasource have the same
    -- "type". It will be more beneficial to be able to instead map requests
    -- to their names (ie, data constructor) - but there's no cheap way of doing
    -- that.
    ty :: TypeRep
    !ty = typeOf1 (undefined :: r a)

-- Filter all keys with 0 fetches. Since ReqCountMap is a 2-level map, we need
-- nested filter operations.
filterRCMap :: ReqCountMap -> ReqCountMap
filterRCMap (ReqCountMap m) = ReqCountMap $
  Map.filter ((> 0) . Map.size) (Map.filter (> 0) <$> m)

-- Filters the ReqCountMap by default
getMapFromRCMap :: ReqCountMap -> Map Text (Map TypeRep Int)
getMapFromRCMap r
  | ReqCountMap m <- filterRCMap r = m

getSummaryMapFromRCMap :: ReqCountMap -> HashMap.HashMap Text Int
getSummaryMapFromRCMap (ReqCountMap m) = HashMap.fromList
  [ (k, s)
  | (k, v) <- Map.toList m
  , not $ Map.null v
  , let s = sum $ Map.elems v
  , s > 0
  ]
