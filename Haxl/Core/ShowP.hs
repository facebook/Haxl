{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

-- |
-- Most users should import "Haxl.Core" instead of importing this
-- module directly.
--
module Haxl.Core.ShowP
  ( ShowP(..)
  ) where

-- | A class of type constructors for which we can show all
-- parameterizations.
class ShowP f where
  showp :: f a -> String
