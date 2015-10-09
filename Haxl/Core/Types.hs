-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file. An additional grant of patent rights can
-- be found in the PATENTS file.

{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Base types used by all of Haxl. Most users should import
-- "Haxl.Core" instead of importing this module directly.
module Haxl.Core.Types (

  -- * Tracing flags
  Flags(..),
  defaultFlags,
  ifTrace,
  ifReport,

  -- * Statistics
  Stats(..),
  RoundStats(..),
  DataSourceRoundStats(..),
  Microseconds,
  emptyStats,
  numRounds,
  numFetches,
  ppStats,
  ppRoundStats,
  ppDataSourceRoundStats,

  -- * Data fetching
  DataSource(..),
  DataSourceName(..),
  Request,
  BlockedFetch(..),
  PerformFetch(..),

  -- * Result variables
  ResultVar(..),
  newEmptyResult,
  newResult,
  putFailure,
  putResult,
  putSuccess,
  takeResult,
  tryReadResult,
  tryTakeResult,

  -- * Default fetch implementations
  asyncFetch, asyncFetchWithDispatch,
  stubFetch,
  syncFetch,

  -- * Utilities
  except,
  setError,

  ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Function (on)
import Data.Hashable
import Data.HashMap.Strict (HashMap, toList)
import qualified Data.HashMap.Strict as HashMap
import Data.List (intercalate, sortBy)
import Data.Text (Text, unpack)
import Data.Typeable (Typeable)

#if __GLASGOW_HASKELL__ < 708
import Haxl.Core.Util (tryReadMVar)
#endif
import Haxl.Core.Show1
import Haxl.Core.StateStore

-- | Flags that control the operation of the engine.
data Flags = Flags
  { trace :: Int
    -- ^ Tracing level (0 = quiet, 3 = very verbose).
  , report :: Int
    -- ^ Report level (0 = quiet, 1 = # of requests, 2 = time, 3 = # of errors)
  }

defaultFlags :: Flags
defaultFlags = Flags
  { trace = 0
  , report = 1
  }

-- | Runs an action if the tracing level is above the given threshold.
ifTrace :: (Functor m, Monad m) => Flags -> Int -> m a -> m ()
ifTrace flags i = when (trace flags >= i) . void

-- | Runs an action if the report level is above the given threshold.
ifReport :: (Functor m, Monad m) => Flags -> Int -> m a -> m ()
ifReport flags i = when (report flags >= i) . void

type Microseconds = Int

-- | Stats that we collect along the way.
newtype Stats = Stats [RoundStats]
  deriving ToJSON

-- | Pretty-print Stats.
ppStats :: Stats -> String
ppStats (Stats rss) =
  intercalate "\n" [ "Round: " ++ show i ++ " - " ++ ppRoundStats rs
                   | (i, rs) <- zip [(1::Int)..] (reverse rss) ]

-- | Maps data source name to the number of requests made in that round.
-- The map only contains entries for sources that made requests in that
-- round.
data RoundStats = RoundStats
  { roundTime :: Microseconds
  , roundDataSources :: HashMap Text DataSourceRoundStats
  }

-- | Pretty-print RoundStats.
ppRoundStats :: RoundStats -> String
ppRoundStats (RoundStats t dss) =
    show t ++ "us\n"
      ++ unlines [ "  " ++ unpack nm ++ ": " ++ ppDataSourceRoundStats dsrs
                 | (nm, dsrs) <- sortBy (compare `on` fst) (toList dss) ]

instance ToJSON RoundStats where
  toJSON RoundStats{..} = object
    [ "time" .= roundTime
    , "dataSources" .= roundDataSources
    ]

-- | Detailed stats of each data source in each round.
data DataSourceRoundStats = DataSourceRoundStats
  { dataSourceFetches :: Int
  , dataSourceTime :: Maybe Microseconds
  }

-- | Pretty-print DataSourceRoundStats
ppDataSourceRoundStats :: DataSourceRoundStats -> String
ppDataSourceRoundStats (DataSourceRoundStats i t) =
  maybeTime $ show i ++ " fetches"
    where maybeTime = maybe id (\ tm s -> s ++ " (" ++ show tm ++ "us)") t

instance ToJSON DataSourceRoundStats where
  toJSON DataSourceRoundStats{..} = object [k .= v | (k, Just v) <-
    [ ("fetches", Just dataSourceFetches)
    , ("time", dataSourceTime)
    ]]

fetchesInRound :: RoundStats -> Int
fetchesInRound (RoundStats _ hm) =
  sum $ map dataSourceFetches $ HashMap.elems hm

emptyStats :: Stats
emptyStats = Stats []

numRounds :: Stats -> Int
numRounds (Stats rs) = length rs

numFetches :: Stats -> Int
numFetches (Stats rs) = sum (map fetchesInRound rs)

-- | The class of data sources, parameterised over the request type for
-- that data source. Every data source must implement this class.
--
-- A data source keeps track of its state by creating an instance of
-- 'StateKey' to map the request type to its state. In this case, the
-- type of the state should probably be a reference type of some kind,
-- such as 'IORef'.
--
-- For a complete example data source, see
-- <https://github.com/facebook/Haxl/tree/master/example Examples>.
--
class (DataSourceName req, StateKey req, Show1 req) => DataSource u req where

  -- | Issues a list of fetches to this 'DataSource'. The 'BlockedFetch'
  -- objects contain both the request and the 'ResultVar's into which to put
  -- the results.
  fetch
    :: State req
      -- ^ Current state.
    -> Flags
      -- ^ Tracing flags.
    -> u
      -- ^ User environment.
    -> [BlockedFetch req]
      -- ^ Requests to fetch.
    -> PerformFetch
      -- ^ Fetch the data; see 'PerformFetch'.

class DataSourceName req where
  -- | The name of this 'DataSource', used in tracing and stats. Must
  -- take a dummy request.
  dataSourceName :: req a -> Text

-- The 'Show1' class is a workaround for the fact that we can't write
-- @'Show' (req a)@ as a superclass of 'DataSource', without also
-- parameterizing 'DataSource' over @a@, which is a pain (I tried
-- it). 'Show1' seems fairly benign, though.

-- | A convenience only: package up 'Eq', 'Hashable', 'Typeable', and 'Show'
-- for requests into a single constraint.
type Request req a =
  ( Eq (req a)
  , Hashable (req a)
  , Typeable (req a)
  , Show (req a)
  , Show a
  )

-- | A data source can fetch data in one of two ways.
--
--   * Synchronously ('SyncFetch'): the fetching operation is an
--     @'IO' ()@ that fetches all the data and then returns.
--
--   * Asynchronously ('AsyncFetch'): we can do something else while the
--     data is being fetched. The fetching operation takes an @'IO' ()@ as
--     an argument, which is the operation to perform while the data is
--     being fetched.
--
-- See 'syncFetch' and 'asyncFetch' for example usage.
--
data PerformFetch
  = SyncFetch  (IO ())
  | AsyncFetch (IO () -> IO ())

-- Why does AsyncFetch contain a `IO () -> IO ()` rather than the
-- alternative approach of returning the `IO` action to retrieve the
-- results, which might seem better: `IO (IO ())`?  The point is that
-- this allows the data source to acquire resources for the purpose of
-- this fetching round using the standard `bracket` pattern, so it can
-- ensure that the resources acquired are properly released even if
-- other data sources fail.

-- | A 'BlockedFetch' is a pair of
--
--   * The request to fetch (with result type @a@)
--
--   * A 'ResultVar' to store either the result or an error
--
-- We often want to collect together multiple requests, but they return
-- different types, and the type system wouldn't let us put them
-- together in a list because all the elements of the list must have the
-- same type. So we wrap up these types inside the 'BlockedFetch' type,
-- so that they all look the same and we can put them in a list.
--
-- When we unpack the 'BlockedFetch' and get the request and the 'ResultVar'
-- out, the type system knows that the result type of the request
-- matches the type parameter of the 'ResultVar', so it will let us take the
-- result of the request and store it in the 'ResultVar'.
--
data BlockedFetch r = forall a. BlockedFetch (r a) (ResultVar a)

-- | Function for easily setting a fetch to a particular exception
setError :: (Exception e) => (forall a. r a -> e) -> BlockedFetch r -> IO ()
setError e (BlockedFetch req m) = putFailure m (e req)

except :: (Exception e) => e -> Either SomeException a
except = Left . toException

-- | A sink for the result of a data fetch in 'BlockedFetch'
newtype ResultVar a = ResultVar (MVar (Either SomeException a))

-- Why do we need an 'MVar' here?  The reason is that the
-- cache serves two purposes:
--
--  1. To cache the results of requests that were submitted in a previous round.
--
--  2. To remember requests that have been encountered in the current round but
--     are not yet submitted, so that if we see the request again we can make
--     sure that we only submit it once.
--
-- Storing the result as an 'MVar' gives two benefits:
--
--   * We can tell the difference between (1) and (2) by testing whether the
--     'MVar' is empty. See 'Haxl.Fetch.cached'.
--
--   * In the case of (2), we don't have to update the cache again after the
--     current round, and after the round we can read the result of each request
--     from its 'MVar'. All instances of identical requests will share the same
--     'MVar' to obtain the result.

newResult :: a -> IO (ResultVar a)
newResult x = ResultVar <$> newMVar (Right x)

newEmptyResult :: IO (ResultVar a)
newEmptyResult = ResultVar <$> newEmptyMVar

putFailure :: (Exception e) => ResultVar a -> e -> IO ()
putFailure r = putResult r . except

putSuccess :: ResultVar a -> a -> IO ()
putSuccess r = putResult r . Right

putResult :: ResultVar a -> Either SomeException a -> IO ()
putResult (ResultVar var) = putMVar var

takeResult :: ResultVar a -> IO (Either SomeException a)
takeResult (ResultVar var) = takeMVar var

tryReadResult :: ResultVar a -> IO (Maybe (Either SomeException a))
tryReadResult (ResultVar var) = tryReadMVar var

tryTakeResult :: ResultVar a -> IO (Maybe (Either SomeException a))
tryTakeResult (ResultVar var) = tryTakeMVar var

-- Fetch templates

stubFetch
  :: (Exception e) => (forall a. r a -> e)
  -> State r -> Flags -> u -> [BlockedFetch r] -> PerformFetch
stubFetch e _state _flags _si bfs = SyncFetch $ mapM_ (setError e) bfs

-- | Common implementation templates for 'fetch' of 'DataSource'.
--
-- Example usage:
--
-- > fetch = syncFetch MyDS.withService MyDS.retrieve
-- >   $ \service request -> case request of
-- >     This x -> MyDS.fetchThis service x
-- >     That y -> MyDS.fetchThat service y
--
asyncFetchWithDispatch
  :: ((service -> IO ()) -> IO ())
  -- ^ Wrapper to perform an action in the context of a service.

  -> (service -> IO ())
  -- ^ Dispatch all the pending requests

  -> (service -> IO ())
  -- ^ Wait for the results

  -> (forall a. service -> request a -> IO (IO (Either SomeException a)))
  -- ^ Enqueue an individual request to the service.

  -> State request
  -- ^ Currently unused.

  -> Flags
  -- ^ Currently unused.

  -> u
  -- ^ Currently unused.

  -> [BlockedFetch request]
  -- ^ Requests to submit.

  -> PerformFetch

asyncFetch, syncFetch
  :: ((service -> IO ()) -> IO ())
  -- ^ Wrapper to perform an action in the context of a service.

  -> (service -> IO ())
  -- ^ Dispatch all the pending requests and wait for the results

  -> (forall a. service -> request a -> IO (IO (Either SomeException a)))
  -- ^ Submits an individual request to the service.

  -> State request
  -- ^ Currently unused.

  -> Flags
  -- ^ Currently unused.

  -> u
  -- ^ Currently unused.

  -> [BlockedFetch request]
  -- ^ Requests to submit.

  -> PerformFetch

asyncFetchWithDispatch
  withService dispatch wait enqueue _state _flags _si requests =
  AsyncFetch $ \inner -> withService $ \service -> do
    getResults <- mapM (submitFetch service enqueue) requests
    dispatch service
    inner
    wait service
    sequence_ getResults

asyncFetch withService wait enqueue _state _flags _si requests =
  AsyncFetch $ \inner -> withService $ \service -> do
    getResults <- mapM (submitFetch service enqueue) requests
    inner
    wait service
    sequence_ getResults

syncFetch withService dispatch enqueue _state _flags _si requests =
  SyncFetch . withService $ \service -> do
  getResults <- mapM (submitFetch service enqueue) requests
  dispatch service
  sequence_ getResults

-- | Used by 'asyncFetch' and 'syncFetch' to retrieve the results of
-- requests to a service.
submitFetch
  :: service
  -> (forall a. service -> request a -> IO (IO (Either SomeException a)))
  -> BlockedFetch request
  -> IO (IO ())
submitFetch service fetch (BlockedFetch request result)
  = (putResult result =<<) <$> fetch service request
