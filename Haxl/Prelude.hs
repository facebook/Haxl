-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file. An additional grant of patent rights can
-- be found in the PATENTS file.

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Support for using Haxl as a DSL.  This module provides most of
-- the standard Prelude, plus a selection of stuff that makes
-- Haxl client code cleaner and more concise.
--
-- We intend Haxl client code to:
--
--  * Import @Haxl.Prelude@
--
--  * Use @RebindableSyntax@.  This implies @NoImplicitPrelude@, and
--    allows @if@-@then@-@else@ to be used with a monadic condition.
--
--  * Use @OverloadedStrings@  (we use @Text@ a lot)
--
module Haxl.Prelude (
    -- * The Standard Haskell Prelude
    -- | Everything from "Prelude" except 'mapM', 'mapM_',
    -- 'sequence', and 'sequence'
    module Prelude,

    -- * Haxl and Fetching data
    GenHaxl, dataFetch, DataSource, memo,

    -- * Extra Monad and Applicative things
    Applicative(..),
#if __GLASGOW_HASKELL__ < 710
    (<$>),
#endif
    mapM, mapM_, sequence, sequence_, filterM, foldM,
    forM, forM_,
    foldl', sort,
    Monoid(..),
    join,

    -- * Lifted operations
    IfThenElse(..),
    (.>), (.<), (.>=), (.<=),
    (.==), (./=), (.&&), (.||),
    (.++),
    pair,

    -- * Text things
    Text,
    IsString(..),

    -- * Exceptions
    throw, catch, try, withDefault, catchAny,
    HaxlException(..), TransientError(..), LogicError(..),
    NotFound(..), UnexpectedType(..), FetchError(..),
    EmptyList(..), InvalidParameter(..)

  ) where

import Haxl.Core.Types
import Haxl.Core.Exception
import Haxl.Core.Memo
import Haxl.Core.Monad

import Control.Applicative
import Control.Monad (foldM, join)
import Data.List (foldl', sort)
import Data.Text (Text)
import Data.Traversable hiding (forM, mapM, sequence)
import GHC.Exts (IsString(..))
import Prelude hiding (mapM, mapM_, sequence, sequence_)
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif
import Data.Maybe
import Control.Exception (fromException)

infixr 3 .&&
infixr 2 .||
infix  4 .>, .<, .>=, .<=, .==, ./=

-- -----------------------------------------------------------------------------
-- Haxl versions of Haskell Prelude stuff

-- Using overloading and RebindableSyntax to hide the monad as far as
-- possible.

class IfThenElse a b where
  ifThenElse :: a -> b -> b -> b

instance IfThenElse Bool a where
  ifThenElse b t e = if b then t else e

-- The equality constraint is necessary to convince the typechecker that
-- this is valid:
--
-- > if getCountry ip .== "us" then ... else ...
--
instance (u1 ~ u2) => IfThenElse (GenHaxl u1 Bool) (GenHaxl u2 a) where
  ifThenElse fb t e = do
    b <- fb
    if b then t else e

instance Num a => Num (GenHaxl u a) where
  (+)         = liftA2 (+)
  (-)         = liftA2 (-)
  (*)         = liftA2 (*)
  fromInteger = pure . fromInteger
  abs         = liftA abs
  signum      = liftA signum
  negate      = liftA negate

instance Fractional a => Fractional (GenHaxl u a) where
  (/) = liftA2 (/)
  recip = liftA recip
  fromRational = return . fromRational

-- -----------------------------------------------------------------------------
-- Convenience functions for avoiding do-notation boilerplate

-- convention is to prefix the name with a '.'.  We could change this,
-- or even just not provide these at all.

(.>) :: Ord a => GenHaxl u a -> GenHaxl u a -> GenHaxl u Bool
(.>) = liftA2 (Prelude.>)

(.<) :: Ord a => GenHaxl u a -> GenHaxl u a -> GenHaxl u Bool
(.<) = liftA2 (Prelude.<)

(.>=) :: Ord a => GenHaxl u a -> GenHaxl u a -> GenHaxl u Bool
(.>=) = liftA2 (Prelude.>=)

(.<=) :: Ord a => GenHaxl u a -> GenHaxl u a -> GenHaxl u Bool
(.<=) = liftA2 (Prelude.<=)

(.==) :: Eq a => GenHaxl u a -> GenHaxl u a -> GenHaxl u Bool
(.==) = liftA2 (Prelude.==)

(./=) :: Eq a => GenHaxl u a -> GenHaxl u a -> GenHaxl u Bool
(./=) = liftA2 (Prelude./=)

(.++) :: GenHaxl u [a] -> GenHaxl u [a] -> GenHaxl u [a]
(.++) = liftA2 (Prelude.++)

-- short-circuiting Bool operations
(.&&):: GenHaxl u Bool -> GenHaxl u Bool -> GenHaxl u Bool
fa .&& fb = do a <- fa; if a then fb else return False

(.||):: GenHaxl u Bool -> GenHaxl u Bool -> GenHaxl u Bool
fa .|| fb = do a <- fa; if a then return True else fb

pair :: GenHaxl u a -> GenHaxl u b -> GenHaxl u (a, b)
pair = liftA2 (,)

-- -----------------------------------------------------------------------------
-- Applicative traversals

-- | We don't want the monadic 'mapM', because that doesn't do batching.
-- There doesn't seem to be a way to make 'Data.Traversable.mapM' have
-- the right behaviour when used with Haxl, so instead we define 'mapM'
-- to be 'traverse' in Haxl code.
mapM :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
mapM = traverse

forM :: (Traversable t, Applicative f) => t a -> (a -> f b) -> f (t b)
forM = flip mapM

-- | See 'mapM'.
mapM_ :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f ()
mapM_ f t = const () <$> traverse f t

forM_ :: (Traversable t, Applicative f) => t a -> (a -> f b) -> f ()
forM_ = flip mapM_

-- | See 'mapM'.
sequence :: (Traversable t, Applicative f) => t (f a) -> f (t a)
sequence = sequenceA

-- | See 'mapM'.
sequence_ :: (Traversable t, Applicative f) => t (f a) -> f ()
sequence_ t = const () <$> sequenceA t

-- | See 'mapM'.
filterM :: (Applicative f, Monad f) => (a -> f Bool) -> [a] -> f [a]
filterM pred xs = do
  bools <- mapM pred xs
  return [ x | (x,True) <- zip xs bools ]

--------------------------------------------------------------------------------

-- | Runs the given 'GenHaxl' computation, and if it throws a
-- 'TransientError' or 'LogicError' exception (see
-- "Haxl.Core.Exception"), the exception is ignored and the supplied
-- default value is returned instead.
withDefault :: a -> GenHaxl u a -> GenHaxl u a
withDefault d a = catchAny a (return d)

-- | Catch 'LogicError's and 'TransientError's and perform an alternative action
catchAny
  :: GenHaxl u a   -- ^ run this first
  -> GenHaxl u a   -- ^ if it throws 'LogicError' or 'TransientError', run this
  -> GenHaxl u a
catchAny haxl handler =
  haxl `catch` \e ->
    if isJust (fromException e :: Maybe LogicError) ||
       isJust (fromException e :: Maybe TransientError)
      then
        handler
      else
        throw e
