-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file. An additional grant of patent rights can
-- be found in the PATENTS file.

{-# LANGUAGE CPP, OverloadedStrings #-}
module LoadCache where

import Haxl.Core
import ExampleDataSource

#include "LoadCache.txt"
