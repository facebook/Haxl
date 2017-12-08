-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

-- | Internal utilities only.
--
module Haxl.Core.Util
  ( compose
  , textShow
  ) where

import Data.Text (Text)

import qualified Data.Text as Text

-- | Composes a list of endofunctions.
compose :: [a -> a] -> a -> a
compose = foldr (.) id

textShow :: (Show a) => a -> Text
textShow = Text.pack . show
