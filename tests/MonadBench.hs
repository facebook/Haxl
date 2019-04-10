-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

-- | Benchmarking tool for core performance characteristics of the Haxl monad.
{-# LANGUAGE CPP #-}
module MonadBench (main) where

import Control.Monad
import Data.List as List
import Data.Time.Clock
import System.Environment
import System.Exit
import System.IO
import Text.Printf

import Haxl.Prelude as Haxl
import Prelude()

import Haxl.Core.Memo (newMemoWith, runMemo)

import Haxl.Core

import ExampleDataSource

testEnv :: IO (Env () ())
testEnv = do
  exstate <- ExampleDataSource.initGlobalState
  let st = stateSet exstate stateEmpty
  initEnv st ()

main :: IO ()
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
       _ <- foldl andThen (return []) (map listWombats [1.. fromIntegral n])
       return ()
    "tree" -> runHaxl env $ void $ tree n
    -- No memoization
    "memo0" -> runHaxl env $
      Haxl.sequence_ [unionWombats | _ <- [1..n]]
    -- One put, N gets.
    "memo1" -> runHaxl env $
      Haxl.sequence_ [memo (42 :: Int) unionWombats | _ <- [1..n]]
    -- N puts, N gets.
    "memo2" -> runHaxl env $
      Haxl.sequence_ [memo (i :: Int) unionWombats | i <- [1..n]]
    "memo3" ->
      runHaxl env $ do
        ref <- newMemoWith unionWombats
        let c = runMemo ref
        Haxl.sequence_ [c | _ <- [1..n]]
    "memo4" ->
      runHaxl env $ do
        let f = unionWombatsTo
        Haxl.sequence_ [f x | x <- take n $ cycle [100, 200 .. 1000]]
    "memo5" ->
      runHaxl env $ do
        f <- memoize1 unionWombatsTo
        Haxl.sequence_ [f x | x <- take n $ cycle [100, 200 .. 1000]]
    "memo6" ->
      runHaxl env $ do
        let f = unionWombatsFromTo
        Haxl.sequence_ [ f x y
                       | x <- take n $ cycle [100, 200 .. 1000]
                       , let y = x + 1000
                       ]
    "memo7" ->
      runHaxl env $ do
        f <- memoize2 unionWombatsFromTo
        Haxl.sequence_ [ f x y
                       | x <- take n $ cycle [100, 200 .. 1000]
                       , let y = x + 1000
                       ]

    "cc1" -> runHaxl env $
      Haxl.sequence_ [ cachedComputation (ListWombats 1000) unionWombats
                     | _ <- [1..n]
                     ]

    _ -> do
      hPutStrLn stderr $ "syntax: monadbench " ++ concat
        [ "par1"
        , "par2"
        , "seqr"
        , "seql"
        , "memo0"
        , "memo1"
        , "memo2"
        , "memo3"
        , "memo4"
        , "memo5"
        , "memo6"
        , "memo7"
        , "cc1"
        ]
      exitWith (ExitFailure 1)
  t1 <- getCurrentTime
  printf "%10s: %10d reqs: %.2fs\n"
    test n (realToFrac (t1 `diffUTCTime` t0) :: Double)
 where
  -- can't use >>, it is aliased to *> and we want the real bind here
  andThen x y = x >>= const y

tree :: Int -> GenHaxl () () [Id]
tree 0 = listWombats 0
tree n = concat <$> Haxl.sequence
  [ tree (n-1)
  , listWombats (fromIntegral n), tree (n-1)
  ]

unionWombats :: GenHaxl () () [Id]
unionWombats = foldl List.union [] <$> Haxl.mapM listWombats [1..1000]

unionWombatsTo :: Id -> GenHaxl () () [Id]
unionWombatsTo x = foldl List.union [] <$> Haxl.mapM listWombats [1..x]

unionWombatsFromTo :: Id -> Id -> GenHaxl () () [Id]
unionWombatsFromTo x y = foldl List.union [] <$> Haxl.mapM listWombats [x..y]
