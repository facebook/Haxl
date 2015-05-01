{-# LANGUAGE OverloadedStrings #-}
module TestUtils
  ( makeTestEnv
  , expectRoundsWithEnv
  , expectRounds
  , expectFetches
  , testinput
  , id1, id2, id3, id4
  ) where

import TestTypes
import MockTAO

import Data.IORef
import Data.Aeson
import Test.HUnit
import qualified Data.HashMap.Strict as HashMap

import Haxl.Core

import Prelude()
import Haxl.Prelude

testinput :: Object
testinput = HashMap.fromList [
  "A" .= (1 :: Int),
  "B" .= (2 :: Int),
  "C" .= (3 :: Int),
  "D" .= (4 :: Int) ]

id1 :: Haxl Id
id1 = lookupInput "A"

id2 :: Haxl Id
id2 = lookupInput "B"

id3 :: Haxl Id
id3 = lookupInput "C"

id4 :: Haxl Id
id4 = lookupInput "D"

makeTestEnv :: IO (Env UserEnv)
makeTestEnv = do
  tao <- MockTAO.initGlobalState
  let st = stateSet tao stateEmpty
  initEnv st testinput

expectRoundsWithEnv
  :: (Eq a, Show a) => Int -> a -> Haxl a -> Env UserEnv -> Assertion
expectRoundsWithEnv n result haxl env = do
  a <- runHaxl env haxl
  assertEqual "result" result a
  stats <- readIORef (statsRef env)
  assertEqual "rounds" n (numRounds stats)

expectRounds :: (Eq a, Show a) => Int -> a -> Haxl a -> Assertion
expectRounds n result haxl = do
  env <- makeTestEnv
  expectRoundsWithEnv n result haxl env

expectFetches :: (Eq a, Show a) => Int -> Haxl a -> Assertion
expectFetches n haxl = do
  env <- makeTestEnv
  _ <- runHaxl env haxl
  stats <- readIORef (statsRef env)
  assertEqual "fetches" n (numFetches stats)
