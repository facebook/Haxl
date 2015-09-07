{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module ExampleDataSource (
    -- * initialise the state
    initGlobalState,

    -- * requests for this data source
    Id(..), ExampleReq(..),
    countAardvarks,
    listWombats,
  ) where

import Haxl.Prelude
import Prelude ()

import Haxl.Core

import Data.Typeable
import Data.Hashable
import Control.Concurrent
import System.IO

-- Here is an example minimal data source.  Our data source will have
-- two requests:
--
--   countAardvarks :: String -> Haxl Int
--   listWombats    :: Id     -> Haxl [Id]
--
-- First, the data source defines a request type, with one constructor
-- for each request:

newtype Id = Id Int
  deriving (Eq, Ord, Enum, Num, Integral, Real, Hashable, Typeable)

instance Show Id where
  show (Id i) = show i

data ExampleReq a where
  CountAardvarks :: String -> ExampleReq Int
  ListWombats    :: Id     -> ExampleReq [Id]
  deriving Typeable -- requests must be Typeable

-- The request type (ExampleReq) is parameterized by the result type of
-- each request.  Each request might have a different result, so we use a
-- GADT - a data type in which each constructor may have different type
-- parameters. Here CountAardvarks is a request that takes a String
-- argument and its result is Int, whereas ListWombats takes an Id
-- argument and returns a [Id].

-- The request type needs instances for 'Eq1' and 'Hashable1'.  These
-- are like 'Eq' and 'Hashable', but for types with one parameter
-- where the parameter is irrelevant for hashing and equality.
-- These two instances are used to support caching of requests.

-- We need Eq, but we have to derive it with a standalone declaration
-- like this, because plain deriving doesn't work with GADTs.
deriving instance Eq (ExampleReq a)

deriving instance Show (ExampleReq a)

instance Show1 ExampleReq where show1 = show

instance Hashable (ExampleReq a) where
   hashWithSalt s (CountAardvarks a) = hashWithSalt s (0::Int,a)
   hashWithSalt s (ListWombats a)    = hashWithSalt s (1::Int,a)

instance StateKey ExampleReq where
  data State ExampleReq = ExampleState {
        -- in here you can put any state that the
        -- data source needs to maintain throughout the
        -- run.
        }

-- Next we need to define an instance of DataSourceName:

instance DataSourceName ExampleReq where
  dataSourceName _ = "ExampleDataSource"

-- Next we need to define an instance of DataSource:

instance DataSource u ExampleReq where
  -- I'll define exampleFetch below
  fetch = exampleFetch


-- Every data source should define a function 'initGlobalState' that
-- initialises the state for that data source.  The arguments to this
-- function might vary depending on the data source - we might need to
-- pass in resources from the environment, or parameters to set up the
-- data source.
initGlobalState :: IO (State ExampleReq)
initGlobalState = do
  -- initialize the state here.
  return ExampleState { }


-- The most important bit: fetching the data.  The fetching function
-- takes a list of BlockedFetch, which is defined as
--
-- data BlockedFetch r
--   = forall a . BlockedFetch (r a) (ResultVar a)
--
-- That is, each BlockedFetch is a pair of
--
--   - the request to fetch (with result type a)
--   - a ResultVar to store either the result or an error
--
-- The job of fetch is to fetch the data and fill in all the ResultVars.
--
exampleFetch :: State ExampleReq             -- current state
             -> Flags                        -- tracing verbosity, etc.
             -> u                            -- user environment
             -> [BlockedFetch ExampleReq]    -- requests to fetch
             -> PerformFetch                 -- tells the framework how to fetch

exampleFetch _state _flags _user bfs = SyncFetch $ mapM_ fetch1 bfs

  -- There are two ways a data source can fetch data: synchronously or
  -- asynchronously.  See the type 'PerformFetch' in "Haxl.Core.Types" for
  -- details.

fetch1 :: BlockedFetch ExampleReq -> IO ()
fetch1 (BlockedFetch (CountAardvarks "BANG") _) =
  error "BANG"  -- data sources should not throw exceptions, but in
                -- the event that one does, the framework will
                -- propagate the exception to the call site of
                -- dataFetch.
fetch1 (BlockedFetch (CountAardvarks "BANG2") m) = do
  putSuccess m 1
  error "BANG2" -- the exception is propagated even if we have already
                -- put the result with putSuccess
fetch1 (BlockedFetch (CountAardvarks "BANG3") _) = do
  hPutStr stderr "BANG3"
  killThread =<< myThreadId -- an asynchronous exception
fetch1 (BlockedFetch (CountAardvarks str) m) =
  putSuccess m (length (filter (== 'a') str))
fetch1 (BlockedFetch (ListWombats a) r) =
  if a > 999999
    then putFailure r $ FetchError "too large"
    else putSuccess r $ take (fromIntegral a) [1..]


-- Normally a data source will provide some convenient wrappers for
-- its requests:

countAardvarks :: String -> GenHaxl () Int
countAardvarks str = dataFetch (CountAardvarks str)

listWombats :: Id -> GenHaxl () [Id]
listWombats id = dataFetch (ListWombats id)
