-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

-- | Psuedo-parallel operations.  Most users should import "Haxl.Core"
-- instead.
--
module Haxl.Core.Parallel
  ( -- * Parallel operations
    biselect
  , pAnd
  , pOr
  ) where

import Haxl.Core.Monad hiding (catch)

-- -----------------------------------------------------------------------------
-- Parallel operations

-- Bind more tightly than .&&, .||
infixr 5 `pAnd`
infixr 4 `pOr`


biselect :: GenHaxl u w (Either a b)
         -> GenHaxl u w (Either a c)
         -> GenHaxl u w (Either a (b,c))
biselect haxla haxlb = biselect_opt id id Left Right haxla haxlb

{-# INLINE biselect_opt #-}
biselect_opt :: (l -> Either a b)
             -> (r -> Either a c)
             -> (a -> t)
             -> ((b,c) -> t)
             -> GenHaxl u w l
             -> GenHaxl u w r
             -> GenHaxl u w t
biselect_opt discrimA discrimB left right haxla haxlb =
  let go (GenHaxl haxla) (GenHaxl haxlb) = GenHaxl $ \env@Env{..} -> do
        let !senv = speculate env
        ra <- haxla senv
        case ra of
          Done ea ->
            case discrimA ea of
              Left a -> return (Done (left a))
              Right b -> do
                  rb <- haxlb env
                  case rb of
                    Done eb ->
                      case discrimB eb of
                        Left a -> return (Done (left a))
                        Right c -> return (Done (right (b,c)))
                    Throw e -> return (Throw e)
                    Blocked ib haxlb' ->
                      return (Blocked ib
                              (haxlb' :>>= \b' -> go_right b b'))
          Throw e -> return (Throw e)
          Blocked ia haxla' -> do
            rb <- haxlb senv
            case rb of
              Done eb ->
                case discrimB eb of
                  Left a -> return (Done (left a))
                  Right c ->
                     return (Blocked ia
                             (haxla' :>>= \a' -> go_left a' c))
              Throw e -> return (Throw e)
              Blocked ib haxlb' -> do
                i <- newIVar

                let fillIVar = GenHaxl $ \env -> do
                      putIVar i (Ok () NilWrites) env
                      return (Done ())

                iDummyA <- newIVar
                iDummyB <- newIVar
                addJob senv fillIVar iDummyA ia
                addJob senv fillIVar iDummyB ib
                return (Blocked i (Cont (go (toHaxl haxla') (toHaxl haxlb'))))
          -- This goes to some length to make sure that it
          -- wakes up whenever either 'ia' or 'ib' is filled.
          -- The ivar 'i' is used as a synchronisation point
          -- for the whole computation, and we make sure that
          -- whenever 'ia' or 'ib' are filled in then 'i' will
          -- also be filled.

      go_right b eb =
        case discrimB eb of
          Left a -> return (left a)
          Right c -> return (right (b,c))
      go_left ea c =
        case discrimA ea of
          Left a -> return (left a)
          Right b -> return (right (b,c))
  in go haxla haxlb

-- | Parallel version of '(.||)'.  Both arguments are evaluated in
-- parallel, and if either returns 'True' then the other is
-- not evaluated any further.
--
-- WARNING: exceptions may be unpredictable when using 'pOr'.  If one
-- argument returns 'True' before the other completes, then 'pOr'
-- returns 'True' immediately, ignoring a possible exception that
-- the other argument may have produced if it had been allowed to
-- complete.
pOr :: GenHaxl u w Bool -> GenHaxl u w Bool -> GenHaxl u w Bool
pOr x y = biselect_opt discrim discrim left right x y
  where
    discrim True = Left ()
    discrim False = Right ()
    left _ = True
    right _ = False

-- | Parallel version of '(.&&)'.  Both arguments are evaluated in
-- parallel, and if either returns 'False' then the other is
-- not evaluated any further.
--
-- WARNING: exceptions may be unpredictable when using 'pAnd'.  If one
-- argument returns 'False' before the other completes, then 'pAnd'
-- returns 'False' immediately, ignoring a possible exception that
-- the other argument may have produced if it had been allowed to
-- complete.
pAnd :: GenHaxl u w Bool -> GenHaxl u w Bool -> GenHaxl u w Bool
pAnd x y = biselect_opt discrim discrim left right x y
  where
    discrim False = Left ()
    discrim True = Right ()
    left _ = False
    right _ = True
