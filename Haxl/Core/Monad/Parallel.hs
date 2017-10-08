-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file. An additional grant of patent rights can
-- be found in the PATENTS file.

{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Haxl.Core.Monad.Parallel
  ( -- * Parallel operations
    pAnd
  , pOr
  ) where

import Haxl.Core.Monad

-- -----------------------------------------------------------------------------
-- Parallel operations

-- Bind more tightly than .&&, .||
infixr 5 `pAnd`
infixr 4 `pOr`

-- | Parallel version of '(.||)'.  Both arguments are evaluated in
-- parallel, and if either returns 'True' then the other is
-- not evaluated any further.
--
-- WARNING: exceptions may be unpredictable when using 'pOr'.  If one
-- argument returns 'True' before the other completes, then 'pOr'
-- returns 'True' immediately, ignoring a possible exception that
-- the other argument may have produced if it had been allowed to
-- complete.
pOr :: GenHaxl u Bool -> GenHaxl u Bool -> GenHaxl u Bool
GenHaxl a `pOr` GenHaxl b = GenHaxl $ \env@Env{..} -> do
  let !senv = speculate env
  ra <- a senv
  case ra of
    Done True -> return (Done True)
    Done False -> b env  -- not speculative
    Throw _ -> return ra
    Blocked ia a' -> do
      rb <- b senv
      case rb of
        Done True -> return rb
        Done False -> return ra
        Throw _ -> return rb
        Blocked _ b' -> return (Blocked ia (Cont (toHaxl a' `pOr` toHaxl b')))
          -- Note [pOr Blocked/Blocked]
          -- This will only wake up when ia is filled, which
          -- is whatever the left side was waiting for.  This is
          -- suboptimal because the right side might wake up first,
          -- but handling this non-determinism would involve a much
          -- more complicated implementation here.

-- | Parallel version of '(.&&)'.  Both arguments are evaluated in
-- parallel, and if either returns 'False' then the other is
-- not evaluated any further.
--
-- WARNING: exceptions may be unpredictable when using 'pAnd'.  If one
-- argument returns 'False' before the other completes, then 'pAnd'
-- returns 'False' immediately, ignoring a possible exception that
-- the other argument may have produced if it had been allowed to
-- complete.
pAnd :: GenHaxl u Bool -> GenHaxl u Bool -> GenHaxl u Bool
GenHaxl a `pAnd` GenHaxl b = GenHaxl $ \env@Env{..} -> do
  let !senv = speculate env
  ra <- a senv
  case ra of
    Done False -> return (Done False)
    Done True -> b env
    Throw _ -> return ra
    Blocked ia a' -> do
      rb <- b senv
      case rb of
        Done False -> return rb
        Done True -> return ra
        Throw _ -> return rb
        Blocked _ b' -> return (Blocked ia (Cont (toHaxl a' `pAnd` toHaxl b')))
         -- See Note [pOr Blocked/Blocked]
