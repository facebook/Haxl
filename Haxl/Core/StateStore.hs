-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}

-- |
-- Most users should import "Haxl.Core" instead of importing this
-- module directly.
--
module Haxl.Core.StateStore
  ( StateKey(..)
  , StateStore
  , stateGet
  , stateSet
  , stateEmpty
  ) where

import Data.Map (Map)
import qualified Data.Map.Strict as Map
#if __GLASGOW_HASKELL__ < 804
import Data.Monoid
#endif
import Data.Typeable
import Unsafe.Coerce

-- | 'StateKey' maps one type to another type. A type that is an
-- instance of 'StateKey' can store and retrieve information from a
-- 'StateStore'.
--
class Typeable f => StateKey (f :: * -> *) where
  data State f

  -- | We default this to typeOf1, but if f is itself a complex type that is
  -- already applied to some paramaters, we want to be able to use the same
  -- state by using typeOf2, etc
  getStateType :: Proxy f -> TypeRep
  getStateType = typeRep

-- | The 'StateStore' maps a 'StateKey' to the 'State' for that type.
newtype StateStore = StateStore (Map TypeRep StateStoreData)

#if __GLASGOW_HASKELL__ >= 804
instance Semigroup StateStore where
  (<>) = mappend
#endif

instance Monoid StateStore where
  mempty = stateEmpty
  -- Left-biased union
  mappend (StateStore m1) (StateStore m2) = StateStore $ m1 <> m2

-- | Encapsulates the type of 'StateStore' data so we can have a
-- heterogeneous collection.
data StateStoreData = forall f. StateKey f => StateStoreData (State f)

-- | A `StateStore` with no entries.
stateEmpty :: StateStore
stateEmpty = StateStore Map.empty

-- | Inserts a `State` in the `StateStore` container.
stateSet :: forall f . StateKey f => State f -> StateStore -> StateStore
stateSet st (StateStore m) =
  StateStore (Map.insert (getStateType (Proxy :: Proxy f)) (StateStoreData st) m)

-- | Retrieves a `State` from the `StateStore` container.
stateGet :: forall r . StateKey r => StateStore -> Maybe (State r)
stateGet (StateStore m) =
  case Map.lookup ty m of
     Nothing -> Nothing
     Just (StateStoreData (st :: State f))
       | getStateType (Proxy :: Proxy f) == ty -> Just (unsafeCoerce st)
       | otherwise             -> Nothing
          -- the dynamic type check here should be unnecessary, but if
          -- there are bugs in `Typeable` or `Map` then we'll get an
          -- error instead of a crash.  The overhead is a few percent.
 where
  ty = getStateType (Proxy :: Proxy r)
