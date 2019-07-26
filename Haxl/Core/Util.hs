-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

-- | Internal utilities only.
--
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Haxl.Core.Util
  ( compose
  , textShow
  , trace_
  ) where

import Data.Text (Text)
import Debug.Trace (trace)
import qualified Data.Text as Text

-- | Composes a list of endofunctions.
compose :: [a -> a] -> a -> a
compose = foldr (.) id

textShow :: (Show a) => a -> Text
textShow = Text.pack . show

-- | This function can be used to trace a bunch of lines to stdout when
-- debugging haxl core.
trace_ :: String -> a -> a
trace_ _ = id
--trace_ = Debug.Trace.trace
