-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file. An additional grant of patent rights can
-- be found in the PATENTS file.

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Bucketing requests by 'DataSource'.
--
-- When a request is issued by the client via 'dataFetch', it is placed
-- in the 'RequestStore'. When we are ready to fetch the current batch
-- of requests, the 'contents' operation extracts the fetches, bucketed
-- by 'DataSource'.
--
-- This module is provided for access to Haxl internals only; most
-- users should not need to import it.
module Haxl.Core.RequestStore (
    BlockedFetches(..), RequestStore,
    noRequests, addRequest, contents
  ) where

import Haxl.Core.Types
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Typeable
import Unsafe.Coerce

-- | A container for multiple 'BlockedFetch' objects.
newtype RequestStore u = RequestStore (Map TypeRep (BlockedFetches u))
  -- Since we don't know which data sources we will be using, the store
  -- is dynamically-typed.  It maps the TypeRep of the request to the
  -- 'BlockedFetches' for that 'DataSource'.

-- | A batch of 'BlockedFetch' objects for a single 'DataSource'
data BlockedFetches u =
  forall r. (DataSource u r) => BlockedFetches [BlockedFetch r]

-- | A new empty 'RequestStore'.
noRequests :: RequestStore u
noRequests = RequestStore Map.empty

-- | Adds a 'BlockedFetch' to a 'RequestStore'.
addRequest
  :: forall u r. (DataSource u r)
  => BlockedFetch r -> RequestStore u -> RequestStore u
addRequest bf (RequestStore m) =
  RequestStore $ Map.insertWith combine ty (BlockedFetches [bf]) m
 where
  combine :: BlockedFetches u -> BlockedFetches u -> BlockedFetches u
  combine _ (BlockedFetches bfs)
    | typeOf1 (getR bfs) == ty = BlockedFetches (unsafeCoerce bf:bfs)
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
  ty = typeOf1 (undefined :: r a)

-- | Retrieves the whole contents of the 'RequestStore'.
contents :: RequestStore u -> [BlockedFetches u]
contents (RequestStore m) = Map.elems m
