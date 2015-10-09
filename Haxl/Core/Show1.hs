-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file. An additional grant of patent rights can
-- be found in the PATENTS file.

-- Most users should import "Haxl.Core" instead of importing this
-- module directly.
module Haxl.Core.Show1
  ( Show1(..)
  ) where

-- | A class of type constructors for which we can show all
-- parameterizations.
class Show1 f where
  show1 :: f a -> String
