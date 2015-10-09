-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file. An additional grant of patent rights can
-- be found in the PATENTS file.

module MonadBench (main) where

import Control.Monad
import Data.Time.Clock
import System.Environment
import System.Exit
import System.IO
import Text.Printf

import Haxl.Prelude as Haxl
import Prelude()

import Haxl.Core

import ExampleDataSource

testEnv :: IO (Env ())
testEnv = do
  exstate <- ExampleDataSource.initGlobalState
  let st = stateSet exstate stateEmpty
  initEnv st ()

main = do
  [test,n_] <- getArgs
  let n = read n_
  env <- testEnv
  t0 <- getCurrentTime
  case test of
    -- parallel, identical queries
    "par1" -> runHaxl env $
       Haxl.sequence_ (replicate n (listWombats 3))
    -- parallel, distinct queries
    "par2" -> runHaxl env $
       Haxl.sequence_ (map listWombats [1..fromIntegral n])
    -- sequential, identical queries
    "seqr" -> runHaxl env $
       foldr andThen (return ()) (replicate n (listWombats 3))
    -- sequential, left-associated, distinct queries
    "seql" -> runHaxl env $ do
       foldl andThen (return []) (map listWombats [1.. fromIntegral n])
       return ()
    "tree" -> runHaxl env $ void $ tree n
    _ -> do
      hPutStrLn stderr "syntax: monadbench par1|par2|seqr|seql NUM"
      exitWith (ExitFailure 1)
  t1 <- getCurrentTime
  printf "%d reqs: %.2fs\n" n (realToFrac (t1 `diffUTCTime` t0) :: Double)
 where
  -- can't use >>, it is aliased to *> and we want the real bind here
  andThen x y = x >>= const y

tree :: Int -> GenHaxl () [Id]
tree 0 = listWombats 0
tree n = concat <$> Haxl.sequence
  [ tree (n-1)
  , listWombats (fromIntegral n), tree (n-1)
  ]
