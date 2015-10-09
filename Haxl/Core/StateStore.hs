-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file. An additional grant of patent rights can
-- be found in the PATENTS file.

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}

-- | Most users should import "Haxl.Core" instead of importing this
-- module directly.
module Haxl.Core.StateStore (
    StateKey(..), StateStore, stateGet, stateSet, stateEmpty
  ) where

import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Typeable
import Unsafe.Coerce

-- | 'StateKey' maps one type to another type. A type that is an
-- instance of 'StateKey' can store and retrieve information from a
-- 'StateStore'.
--
#if __GLASGOW_HASKELL__ >= 708
class Typeable f => StateKey (f :: * -> *) where
  data State f
#else
class Typeable1 f => StateKey (f :: * -> *) where
  data State f
#endif

-- | The 'StateStore' maps a 'StateKey' to the 'State' for that type.
newtype StateStore = StateStore (Map TypeRep StateStoreData)

-- | Encapsulates the type of 'StateStore' data so we can have a
-- heterogeneous collection.
data StateStoreData = forall f. StateKey f => StateStoreData (State f)

-- | A `StateStore` with no entries.
stateEmpty :: StateStore
stateEmpty = StateStore Map.empty

-- | Inserts a `State` in the `StateStore` container.
stateSet :: forall f . StateKey f => State f -> StateStore -> StateStore
stateSet st (StateStore m) =
  StateStore (Map.insert (getType st) (StateStoreData st) m)

-- | Retrieves a `State` from the `StateStore` container.
stateGet :: forall r . StateKey r => StateStore -> Maybe (State r)
stateGet (StateStore m) =
  case Map.lookup ty m of
     Nothing -> Nothing
     Just (StateStoreData st)
       | getType st == ty  -> Just (unsafeCoerce st)
       | otherwise         -> Nothing
          -- the dynamic type check here should be unnecessary, but if
          -- there are bugs in `Typeable` or `Map` then we'll get an
          -- error instead of a crash.  The overhead is a few percent.
 where
  ty = getType (undefined :: State r)

-- | Returns the 'TypeRep' associated with a particular 'State'.
getType :: forall f . StateKey f => State f -> TypeRep
getType _ = typeOf1 (undefined :: f a)
