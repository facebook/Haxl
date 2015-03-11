-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file. An additional grant of patent rights can
-- be found in the PATENTS file.

{-# LANGUAGE CPP #-}

module Haxl.Core.Util
  ( compose
  , textShow
  , tryReadMVar
  ) where

#if __GLASGOW_HASKELL__ >= 708
import Control.Concurrent (tryReadMVar)
#else
import Control.Concurrent
#endif

import Data.Text (Text)

import qualified Data.Text as Text

-- | Composes a list of endofunctions.
compose :: [a -> a] -> a -> a
compose = foldr (.) id

#if __GLASGOW_HASKELL__ < 708
tryReadMVar :: MVar a -> IO (Maybe a)
tryReadMVar m = do
  mb <- tryTakeMVar m
  case mb of
    Nothing -> return Nothing
    Just a -> putMVar m a >> return (Just a)
#endif

textShow :: (Show a) => a -> Text
textShow = Text.pack . show
