{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

module EquivDataSource (
  MyData (..)
  , getSimilar
  , initGlobalState
  ) where

import           Haxl.Prelude
import           Prelude                 ()

import           Haxl.Core
import           Haxl.Core.Monad         (dataFetchEquiv)

import           Control.Concurrent.MVar
import           Data.Hashable
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Data.Typeable

-- Example data source using equivalent requests.  The data source
-- contains some numbers.  Calling @getSimilar x@ queries if there is
-- a number stored that is equal to @x@ modulo @7@. If so, this number
-- is returned, if not, @x@ itself is stored (and returned).
--
-- This allows to perform a restricted class of write operations that
-- can safely be reordered by Haxl.
--
-- This is a simplified version of the real-world usecase, which
-- involves categorising pieces of data using some minhash-like method
-- to measure similarity.

newtype MyKey = MyKey Int
  deriving (Eq, Ord, Hashable, Typeable, Show)
newtype MyData = MyData Int
  deriving (Eq, Hashable, Typeable, Show)

data MyReq a where
  EnterOrRetrieveData :: MyData -> MyReq MyData
  RetrieveData :: MyKey -> MyReq MyData
  deriving Typeable

key :: MyData -> MyKey
key (MyData x) = MyKey (x `mod` 7)

deriving instance Eq (MyReq a)
deriving instance Show (MyReq a)
instance Show1 MyReq where show1 = show
instance Hashable (MyReq a) where
  hashWithSalt s (EnterOrRetrieveData x) = hashWithSalt s (0::Int, x)
  hashWithSalt s (RetrieveData x)        = hashWithSalt s (1::Int, x)

instance StateKey MyReq where
  data State MyReq = MyState { myData :: MVar (Map MyKey MyData) }

instance DataSourceName MyReq where
  dataSourceName _ = "Datasource with equivalent requests"

instance DataSource u MyReq where
  fetch = myFetch

initGlobalState :: IO (State MyReq)
initGlobalState = do
  myMVar <- newMVar Map.empty
  return (MyState myMVar)

myFetch :: State MyReq -> Flags -> u -> [BlockedFetch MyReq] -> PerformFetch
myFetch state _flags _user bfs = SyncFetch $ mapM_ (fetch1 state) bfs

fetch1 :: State MyReq -> BlockedFetch MyReq -> IO ()
fetch1 state (BlockedFetch (EnterOrRetrieveData val) m) =
  modifyMVar_ (myData state) $ \valMap ->
  case Map.lookup k valMap of
    Nothing ->
      putSuccess m val
      >> return (Map.insert k val valMap)
    Just val' ->
      putSuccess m val'
      >> return valMap
  where k = key val

fetch1 state (BlockedFetch (RetrieveData k) m) = do
  valMap <- readMVar (myData state)
  case Map.lookup k valMap of
    Just val -> putSuccess m val
    Nothing -> putFailure m (FetchError "This should not be possible.")


getSimilar :: MyData -> (GenHaxl ()) MyData
getSimilar =
  let equiv :: MyReq a -> MyReq a -> Bool
      equiv (EnterOrRetrieveData x) (EnterOrRetrieveData y) = key x == key y
      equiv _ _ = error "impossible"
      representative :: MyReq a -> MyReq a
      representative (EnterOrRetrieveData x) = RetrieveData (key x)
      representative _ = error "impossible"
  in dataFetchEquiv equiv representative . EnterOrRetrieveData
