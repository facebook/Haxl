-- Copyright 2004-present Facebook. All Rights Reserved.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Haxl.Core.CallGraph where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
#if __GLASGOW_HASKELL__ < 804
import Data.Monoid
#endif
import Data.Text (Text)
import qualified Data.Text as Text

type ModuleName = Text

-- | An unqualified function
type Function = Text

-- | A qualified function
data QualFunction = QualFunction ModuleName Function deriving (Eq, Ord)

instance Show QualFunction where
  show (QualFunction mn nm) = Text.unpack $ mn <> Text.pack "." <> nm

-- | Represents an edge between a parent function which calls a child function
-- in the call graph
type FunctionCall = (QualFunction, QualFunction)

-- | An edge list which represents the dependencies between function calls
type CallGraph = ([FunctionCall], Map QualFunction Text)

-- | Used as the root of all function calls
mainFunction :: QualFunction
mainFunction = QualFunction "MAIN" "main"

emptyCallGraph :: CallGraph
emptyCallGraph = ([], Map.empty)
