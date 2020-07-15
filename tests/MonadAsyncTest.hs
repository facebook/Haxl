-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MonadAsyncTest (tests) where
import Haxl.Core
import Test.HUnit hiding (State)
import Control.Concurrent
import Control.Exception as Exception
import Control.Monad
import Haxl.Core.Monad (unsafeLiftIO)
import System.IO.Unsafe
import Data.Hashable
import Data.IORef
import Data.Text (Text)

newtype SimpleWrite = SimpleWrite Text deriving (Eq, Show)

{-# NOINLINE shouldThrowRef #-}
shouldThrowRef :: IORef Bool
shouldThrowRef = unsafePerformIO (newIORef False)

-- | This datasource contains "bad" code which can throw at the wrong
-- moment.
data ThrowableSleep a where
  Sleep :: Int -> ThrowableSleep Int

deriving instance Eq (ThrowableSleep a)
deriving instance Show (ThrowableSleep a)

instance ShowP ThrowableSleep where showp = show

instance Hashable (ThrowableSleep a) where
  hashWithSalt s (Sleep n) = hashWithSalt s n

instance StateKey ThrowableSleep where
  data State ThrowableSleep = ThrowableSleepState

initDataSource :: IO (State ThrowableSleep)
initDataSource = return ThrowableSleepState

instance DataSourceName ThrowableSleep where
  dataSourceName _ = "ThrowableSleep"

instance DataSource u ThrowableSleep where
  fetch _state _flags _u = BackgroundFetch $ \bfs -> forM_ bfs fill
    where
    fill :: BlockedFetch ThrowableSleep -> IO ()
    fill (BlockedFetch (Sleep n) rv) = do
      _ <- forkFinally
        (do
          threadDelay (n*1000)
          return n
        )
        (\res -> do
          shouldThrow <- atomicModifyIORef' shouldThrowRef (\s -> (False, s))
          -- Simulate case when datasource throws before putting Result into
          -- completions queue.
          when shouldThrow $ do
            throwIO $ ErrorCall "datasource threw an exception"
          -- In case the datasource throws before this point, there'll be
          -- nothing to put the result to the queue of 'completions', and
          -- therefore Haxl would block indefinitely.
          --
          -- Note that Haxl tries to catch datasource exceptions and put the
          -- "exception result" into `completions` using `wrapFetchInCatch`
          -- function. However that doesn't work in this case because the
          -- datasource throws in a separate thread.
          putResultFromChildThread rv res
        )
      return ()

tests :: Test
tests = TestList
  [ TestLabel "exceptionTest" exceptionTest
  ]

mkTestEnv :: IO (Env () SimpleWrite)
mkTestEnv = do
  st <- initDataSource
  initEnv (stateSet st stateEmpty) ()

exceptionTest :: Test
exceptionTest = TestCase $ do
  e <- mkTestEnv

  let
      fet (n :: Int) (st :: Bool )= do
        x <- dataFetch (Sleep (fromIntegral n))
        unsafeLiftIO $ writeIORef shouldThrowRef st
        y <- dataFetch (Sleep (fromIntegral x*2))
        return (x+y)

  r1 :: (Either Exception.SomeException Int)
    <- Exception.try $ runHaxl e $ fet 10 True

  -- Datasources are responsible for putting the fetched result into the
  -- completions queue. If for some reason they fail to do so, Haxl throws a
  -- LogicBug since the scheduler is still expecting some request(s) to
  -- be completed.
  case r1 of
    Left ex | Just (LogicBug _) <- Exception.fromException ex -> return ()
    _ -> assertFailure "r1 computation did not fail with Logic Bug!"

  -- Sanitize the env to get rid of all empty IVars
  -- While this test examines the case when there's an exception in the Haxl
  -- datasource itself, a similar behavior will occur in case an async
  -- exception is thrown to the Haxl scheduler thread.
  e' <- sanitizeEnv e

  r2 :: (Either Exception.SomeException Int)
    <- Exception.try $ runHaxl e' $ fet 10 False
  case r2 of
    Right _ -> return ()
    Left ex | Just (LogicBug _) <- Exception.fromException ex -> do
                assertFailure $ "bad exception in r2: " ++ show ex
    Left _ -> return ()
