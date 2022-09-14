-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RebindableSyntax #-}

module CoreTests where

import Haxl.Prelude
import Prelude ()

import Haxl.Core

import Test.HUnit

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List

import Control.Exception (Exception(..))

import ExampleDataSource

testEnv = do
  -- To use a data source, we need to initialize its state:
  exstate <- ExampleDataSource.initGlobalState

  -- And create a StateStore object containing the states we need:
  let st = stateSet exstate stateEmpty

  -- Create the Env:
  initEnv st () :: IO (Env () ())

useless :: String -> GenHaxl u w Bool
useless _ = throw (NotFound "ha ha")

exceptions :: Assertion
exceptions =
  do
    en <- emptyEnv () :: IO (Env () ())
    a <- runHaxl en $ try (useless "input")
    assertBool "NotFound -> HaxlException" $
      isLeft (a :: Either HaxlException Bool)

    b <- runHaxl en $ try (useless "input")
    assertBool "NotFound -> Logic Error" $
      isLeft (b :: Either LogicError Bool)

    c <- runHaxl en $ try (useless "input")
    assertBool "NotFound -> NotFound" $
      isLeft (c :: Either NotFound Bool)

    -- Make sure TransientError -doesn't- catch our NotFound
    d <- runHaxl en $
             (useless "input"
              `catch` \TransientError{} -> return False)
             `catch` \LogicError{} -> return True
    assertBool "Transient != NotFound" d

    -- test catch
    e <- runHaxl en $
         throw (NotFound "haha") `catch` \NotFound{} -> return True
    assertBool "catch1" e

    f <- runHaxl en $
         throw (NotFound "haha") `catch` \LogicError{} -> return True
    assertBool "catch2" f

    -- test catchIf
    let transientOrNotFound e
          | Just TransientError{} <- fromException e  = True
          | Just NotFound{} <- fromException e  = True
          | otherwise = False

    e <- runHaxl en $
         catchIf transientOrNotFound (throw (NotFound "haha")) $ \_ ->
           return True
    assertBool "catchIf1" e

    e <- runHaxl en $
         catchIf transientOrNotFound (throw (FetchError "haha")) $ \_ ->
           return True
    assertBool "catchIf2" e

    e <- runHaxl en $
           (catchIf transientOrNotFound (throw (CriticalError "haha")) $ \_ ->
              return True)
            `catch` \InternalError{} -> return False
    assertBool "catchIf2" (not e)

    -- test tryToHaxlException
    e <- runHaxl en $ tryToHaxlException $ head []
    assertBool "tryToHaxlException1" $
      case e of
        Left ex | Just NonHaxlException{} <- fromException (toException ex)
          -> True
        _ -> False

    env <- testEnv
    e <- runHaxl env $ tryToHaxlException $ do
      xs <- listWombats 3
      return $! length xs `quot` 0
    print e
    assertBool "tryToHaxlException1" $
      case e of
        Left ex | Just NonHaxlException{} <- fromException (toException ex)
          -> True
        _ -> False

    env <- testEnv
    e <- runHaxl env $ mapM tryToHaxlException
      [ do xs <- listWombats 3; return $! length xs `quot` 0
      , head []
      ]
    print e
    assertBool "tryToHaxlException2" $
      case e of
        [Left ex1, Left ex2]
           | "divide" `isInfixOf` show ex1
           , "head" `isInfixOf` show ex2 -> True
        _ -> False
  where
  isLeft Left{} = True
  isLeft _ = False


-- This is mostly a compile test, to make sure all the plumbing
-- makes the compiler happy.
base :: (Exception a) => a -> IO HaxlException
base e = do
  en <- emptyEnv () :: IO (Env () ())
  runHaxl en $ throw e `catch` \x -> return x

printing :: Assertion
printing = do
  a <- base $ NotFound "notfound!"
  print a

  b <- base $ CriticalError "ohthehumanity!"
  print b

  c <- base $ FetchError "timeout!"
  print c

  BS.putStrLn $ encode a
  BS.putStrLn $ encode b
  BS.putStrLn $ encode c


withEnvTest :: Test
withEnvTest = TestLabel "withEnvTest" $ TestCase $ do
  exstate <- ExampleDataSource.initGlobalState
  e <- initEnv (stateSet exstate stateEmpty) False :: IO (Env Bool ())
  b <- runHaxl e $ withEnv e { userEnv = True } $ env userEnv
  assertBool "withEnv1" b
  e <- initEnv (stateSet exstate stateEmpty) False :: IO (Env Bool ())
  b <- runHaxl e $ withEnv e { userEnv = True } $ do
    _ <- countAardvarks "aaa"
    env userEnv
  assertBool "withEnv2" b
  e <- initEnv (stateSet exstate stateEmpty) False :: IO (Env Bool ())
  b <- runHaxl e $ withEnv e { userEnv = True } $ do
    memo ("xxx" :: Text) $ do
      _ <- countAardvarks "aaa"
      env userEnv
  assertBool "withEnv3" b
  e <- initEnv (stateSet exstate stateEmpty) False :: IO (Env Bool ())
  b <- runHaxl e $
    withEnv e { userEnv = True } $ do
      memo ("yyy" :: Text) $ do
        _ <- countAardvarks "aaa"
        _ <- countAardvarks "bbb"
        env userEnv
  assertBool "withEnv4" b


tests = TestList
  [ TestLabel "exceptions" $ TestCase exceptions,
    TestLabel "print_stuff" $ TestCase printing,
    TestLabel "withEnv" $ withEnvTest
  ]
