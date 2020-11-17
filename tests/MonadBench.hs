-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

-- | Benchmarking tool for core performance characteristics of the Haxl monad.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo, RecordWildCards #-}
module MonadBench (main) where

import Control.Monad
import Data.List as List
import Data.Maybe
import Data.Time.Clock
import Options.Applicative
import System.Exit
import System.IO
import Text.Printf

import Haxl.Prelude as Haxl
import Prelude()

import Haxl.Core
import Haxl.Core.Util

import ExampleDataSource

newtype SimpleWrite = SimpleWrite Text deriving (Eq, Show)

testEnv :: Int -> IO (Env () SimpleWrite)
testEnv report = do
  exstate <- ExampleDataSource.initGlobalState
  let st = stateSet exstate stateEmpty
  env <- initEnv st ()
  return env { flags = (flags env) { report = report } }

type Test = (String, Int, Int -> GenHaxl () SimpleWrite ())

testName :: Test -> String
testName (t,_,_) = t

allTests :: [Test]
allTests =
    -- parallel, identical queries
  [ ("par1", large, \n -> Haxl.sequence_ (replicate n (listWombats 3)))
    -- parallel, distinct queries
  , ("par2", medium, \n ->
        Haxl.sequence_ (map listWombats [1..fromIntegral n]))
    -- sequential, identical queries
  , ("seqr", huge, \n ->
        foldr andThen (return ()) (replicate n (listWombats 3)))
    -- sequential, left-associated, distinct queries
  , ("seql", medium, \n -> do
       _ <- foldl andThen (return []) (map listWombats [1.. fromIntegral n])
       return ())
    -- No memoization
  , ("memo0", small, \n -> Haxl.sequence_ [unionWombats | _ <- [1..n]])
    -- One put, N gets.
  , ("memo1", medium_large, \n ->
        Haxl.sequence_ [memo (42 :: Int) unionWombats | _ <- [1..n]])
    -- N puts, N gets.
  , ("memo2", small, \n ->
        Haxl.sequence_ [memo (i :: Int) unionWombats | i <- [1..n]])
  , ("memo3", medium_large, \n -> do
        ref <- newMemoWith unionWombats
        let c = runMemo ref
        Haxl.sequence_ [c | _ <- [1..n]])
  , ("memo4", small, \n -> do
        let f = unionWombatsTo
        Haxl.sequence_ [f x | x <- take n $ cycle [100, 200 .. 1000]])
  , ("memo5", medium_large, \n -> do
        f <- memoize1 unionWombatsTo
        Haxl.sequence_ [f x | x <- take n $ cycle [100, 200 .. 1000]])
  , ("memo6", small, \n -> do
        let f = unionWombatsFromTo
        Haxl.sequence_ [ f x y
                       | x <- take n $ cycle [100, 200 .. 1000]
                       , let y = x + 1000
                       ])
  , ("memo7", medium_large, \n -> do
        f <- memoize2 unionWombatsFromTo
        Haxl.sequence_ [ f x y
                       | x <- take n $ cycle [100, 200 .. 1000]
                       , let y = x + 1000
                       ])
  , ("cc1", medium_large, \n ->
        Haxl.sequence_ [ cachedComputation (ListWombats 1000) unionWombats
                       | _ <- [1..n]
                       ])
  , ("tree", 20, \n -> void $ tree n (\_ act -> act))
  , ("tree_labels", 20, \n -> void $
      tree n (\n act -> withLabel (textShow n) act))
    -- parallel writes
  , ("write1", large, \n ->
        Haxl.sequence_ (replicate n (tellWrite (SimpleWrite "haha"))))
    -- sequential writes
  , ("write2", huge, \n -> foldr
        andThen
        (return ())
        (replicate n (tellWrite (SimpleWrite "haha"))))
  ]
  where
    huge = large * 10
    large =  medium * 10
    medium_large =  medium * 4
    medium = 200000
    small = 1000

data Options = Options
  { test :: String
  , nOverride :: Maybe Int
  , reportFlag :: Int
  }

runTest :: Options -> Test -> IO ()
runTest Options{..} (t, nDef, act) = do
  let n = fromMaybe nDef nOverride
  env <- testEnv reportFlag
  t0 <- getCurrentTime
  runHaxl env $ act n
  t1 <- getCurrentTime
  printf "%12s: %10d reqs: %.2fs\n"
    t n (realToFrac (t1 `diffUTCTime` t0) :: Double)

optionsParser :: Parser Options
optionsParser = do
  test <- argument str (metavar "TEST")
  reportFlag <- option auto (long "report" <> value 0 <> metavar "REPORT")
  nOverride <- optional $ argument auto (metavar "NUM")
  return Options{..}

main :: IO ()
main = do
  opts@Options{..} <- execParser $ info optionsParser mempty
  let tests = if test == "all"
        then allTests
        else filter ((==) test . testName) allTests
  when
    (null tests)
    (do
        hPutStrLn stderr $ "syntax: monadbench [all|" ++
          intercalate "|" (map testName allTests) ++ "]"
        exitWith (ExitFailure 1))
  Control.Monad.mapM_ (runTest opts) tests

tree
  :: Int
  -> (Int -> GenHaxl () SimpleWrite [Id] -> GenHaxl () SimpleWrite [Id])
  -> GenHaxl () SimpleWrite [Id]
tree 0 wrap = wrap 0 $ listWombats 0
tree n wrap = wrap n $ concat <$> Haxl.sequence
  [ tree (n-1) wrap
  , listWombats (fromIntegral n), tree (n-1) wrap
  ]

unionWombats :: GenHaxl () SimpleWrite [Id]
unionWombats = foldl List.union [] <$> Haxl.mapM listWombats [1..1000]

unionWombatsTo :: Id -> GenHaxl () SimpleWrite [Id]
unionWombatsTo x = foldl List.union [] <$> Haxl.mapM listWombats [1..x]

unionWombatsFromTo :: Id -> Id -> GenHaxl () SimpleWrite [Id]
unionWombatsFromTo x y = foldl List.union [] <$> Haxl.mapM listWombats [x..y]
