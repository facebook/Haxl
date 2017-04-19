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
    State(..), initGlobalState,

    -- * requests for this data source
    FailAfter(..),
  ) where

import Haxl.Prelude
import Prelude ()

import Haxl.Core

import Control.Exception
import Data.Typeable
import Data.Hashable
import Control.Concurrent

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
        }

instance DataSourceName FailAfter where
  dataSourceName _ = "BadDataSource"

instance DataSource u FailAfter where
  -- I'll define exampleFetch below
  fetch state@FailAfterState{..} = asyncFetchAcquireRelease
     (do threadDelay failAcquireDelay; failAcquire)
     (\_ -> do threadDelay failReleaseDelay; failRelease)
     (\_ -> do threadDelay failDispatchDelay; failDispatch)
     (\_ -> do threadDelay failWaitDelay; failWait)
     submit state
   where
     submit :: () -> FailAfter a -> IO (IO (Either SomeException a))
     submit _ (FailAfter t) = do
       threadDelay t
       return (return (Left (toException (FetchError "failed request"))))

-- Every data source should define a function 'initGlobalState' that
-- initialises the state for that data source.  The arguments to this
-- function might vary depending on the data source - we might need to
-- pass in resources from the environment, or parameters to set up the
-- data source.
initGlobalState :: IO (State FailAfter)
initGlobalState = do
  return FailAfterState
    { failAcquireDelay = 0
    , failAcquire = return ()
    , failReleaseDelay = 0
    , failRelease = return ()
    , failDispatchDelay = 0
    , failDispatch = return ()
    , failWaitDelay = 0
    , failWait = return ()
    }
