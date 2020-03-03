-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

-- | A data source that can be made to fail in various ways, for testing

module BadDataSource (
    -- * initialise the state
    State(..), initGlobalState, FetchImpl(..),

    -- * requests for this data source
    FailAfter(..)
  ) where

import Haxl.Prelude
import Prelude ()

import Haxl.Core

import Control.Exception
import Data.Typeable
import Data.Hashable
import Control.Concurrent
import Control.Monad (void)

import GHC.Conc ( PrimMVar )
import Foreign.StablePtr
import Foreign.C.Types ( CInt(..) )

foreign import ccall safe
  hs_try_putmvar :: CInt -> StablePtr PrimMVar -> IO ()

data FetchImpl =
  Async
  | Background
  | BackgroundMVar
  | BackgroundSeq
  | BackgroundPar

data FailAfter a where
  FailAfter :: Int -> FailAfter Int
  deriving Typeable

deriving instance Eq (FailAfter a)
deriving instance Show (FailAfter a)
instance ShowP FailAfter where showp = show

instance Hashable (FailAfter a) where
   hashWithSalt s (FailAfter a) = hashWithSalt s (0::Int,a)

instance StateKey FailAfter where
  data State FailAfter = FailAfterState
         { failAcquireDelay :: Int
         , failAcquire :: IO ()
         , failReleaseDelay :: Int
         , failRelease :: IO ()
         , failDispatchDelay :: Int
         , failDispatch :: IO ()
         , failWaitDelay :: Int
         , failWait :: IO ()
         , failImpl :: FetchImpl
        }

instance DataSourceName FailAfter where
  dataSourceName _ = "BadDataSource"


instance DataSource u FailAfter where
  fetch state@FailAfterState{..}
    | BackgroundSeq <- failImpl = backgroundFetchSeq runOne state
    | BackgroundPar <- failImpl = backgroundFetchPar runOne state
    | Background <- failImpl = backgroundFetchAcquireRelease
        acquire release dispatchbg wait
        submit state
    | BackgroundMVar <- failImpl = backgroundFetchAcquireReleaseMVar
        acquire release dispatchbgMVar wait
        submit state
    | Async <- failImpl = asyncFetchAcquireRelease
       acquire release dispatch wait
       submit state
   where
     acquire = do threadDelay failAcquireDelay; failAcquire
     release _ = do threadDelay failReleaseDelay; failRelease
     dispatch _ = do threadDelay failDispatchDelay; failDispatch
     dispatchBase put = (do
                          failDispatch
                          _ <- mask_ $ forkIO $ finally
                            (threadDelay failDispatchDelay)
                            put
                          return ()) `onException` put
     dispatchbg _ c m = dispatchBase (hs_try_putmvar (fromIntegral c) m)
     dispatchbgMVar _ _ m = dispatchBase (void $ tryPutMVar m ())
     wait _ = do threadDelay failWaitDelay; failWait
     submit :: () -> FailAfter a -> IO (IO (Either SomeException a))
     submit _ (FailAfter t) = do
       threadDelay t
       return (return (Left (toException (FetchError "failed request"))))
     runOne :: FailAfter a -> IO (Either SomeException a)
     runOne r = do
       bracket acquire release $ \s -> do
         dispatch s
         getRes <- submit s r
         wait s
         getRes

initGlobalState :: FetchImpl -> IO (State FailAfter)
initGlobalState impl = do
  return FailAfterState
    { failAcquireDelay = 0
    , failAcquire = return ()
    , failReleaseDelay = 0
    , failRelease = return ()
    , failDispatchDelay = 0
    , failDispatch = return ()
    , failWaitDelay = 0
    , failWait = return ()
    , failImpl = impl
    }
