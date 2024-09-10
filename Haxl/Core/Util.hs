{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

-- | Internal utilities only.
--
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Haxl.Core.Util
  ( atomicallyOnBlocking
  , compose
  , textShow
  , trace_
  ) where

import Data.Text (Text)
import Debug.Trace (trace)
import qualified Data.Text as Text

import Control.Concurrent.STM
import Control.Exception

atomicallyOnBlocking :: Exception e => e -> STM a -> IO a
atomicallyOnBlocking e stm =
  catch (atomically stm)
        (\BlockedIndefinitelyOnSTM -> throw e)

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
