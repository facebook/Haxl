{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Psuedo-parallel operations.  Most users should import "Haxl.Core"
-- instead.
--
module Haxl.Core.Parallel
  ( -- * Parallel operations
    biselect
  , pAnd
  , pOr
  , unsafeChooseFirst
  ) where

import Haxl.Core.Monad hiding (catch, throw)
import Haxl.Core.Exception

import Control.Exception (throw)

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
  let go (GenHaxl haxla) (GenHaxl haxlb) = GenHaxl $ \env -> do
        ra <- haxla env
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
            rb <- haxlb env
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
                addJob env (return ()) i ia
                addJob env (return ()) i ib
                return (Blocked i (Cont (go (toHaxl haxla') (toHaxl haxlb'))))
                -- The code above makes sure that the computation
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

-- | This function takes two haxl computations as input, and returns the
-- output of whichever computation finished first. This is clearly
-- non-deterministic in its output and exception behavior, be careful when
-- using it.
unsafeChooseFirst
  :: GenHaxl u w a
  -> GenHaxl u w b
  -> GenHaxl u w (Either a b)
unsafeChooseFirst x y = biselect_opt discrimx discrimy id right x y
  where
    discrimx :: a -> Either (Either a b) ()
    discrimx a = Left (Left a)

    discrimy :: b -> Either (Either a b) ()
    discrimy b = Left (Right b)

    right _ = throw $ CriticalError
      "unsafeChooseFirst: We should never have a 'Right ()'"
