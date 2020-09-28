-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Types and operations for statistics and profiling.  Most users
-- should import "Haxl.Core" instead of importing this module
-- directly.
--
module Haxl.Core.Stats
  (
  -- * Data-source stats
    Stats(..)
  , CallId
  , FetchStats(..)
  , Microseconds
  , Timestamp
  , getTimestamp
  , emptyStats
  , numFetches
  , ppStats
  , ppFetchStats
  , aggregateFetchBatches

  -- * Profiling
  , Profile(..)
  , ProfileMemo(..)
  , ProfileFetch(..)
  , emptyProfile
  , ProfileKey
  , ProfileLabel
  , ProfileData(..)
  , emptyProfileData
  , AllocCount
  , LabelHitCount

  -- * Allocation
  , getAllocationCounter
  , setAllocationCounter
  ) where

import Data.Aeson
import Data.Function (on)
import Data.Maybe (mapMaybe)
import Data.HashMap.Strict (HashMap)
import Data.Int
import Data.List (intercalate, sortOn, groupBy)
import Data.Ord (Down(..))
import Data.Text (Text)
import Data.Time.Clock.POSIX
import Text.Printf
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import GHC.Conc (getAllocationCounter, setAllocationCounter)

-- ---------------------------------------------------------------------------
-- Measuring time

type Microseconds = Int64
type Timestamp = Microseconds -- since an epoch

getTimestamp :: IO Timestamp
getTimestamp = do
  t <- getPOSIXTime -- for now, TODO better
  return (round (t * 1000000))

-- ---------------------------------------------------------------------------
-- Stats

-- | Stats that we collect along the way.
newtype Stats = Stats [FetchStats]
  deriving (Show, ToJSON, Semigroup, Monoid)

-- | Pretty-print Stats.
ppStats :: Stats -> String
ppStats (Stats rss) =
  intercalate "\n"
    [ "["
    ++ [
      if fetchWasRunning rs
          (minStartTime + (t - 1) * usPerDash)
          (minStartTime + t * usPerDash)
        then fetchSymbol rs
        else '-'
      | t <- [1..numDashes]
      ]
    ++ "] " ++ show i ++ " - " ++ ppFetchStats rs
    | (i, rs) <- zip [(1::Int)..] validFetchStats ]
  where
    isFetchStats FetchStats{} = True
    isFetchStats FetchWait{} = True
    isFetchStats _ = False
    validFetchStats = filter isFetchStats (reverse rss)
    numDashes = 50
    getStart FetchStats{..} = Just fetchStart
    getStart FetchWait{..} = Just fetchWaitStart
    getStart _ = Nothing
    getEnd FetchStats{..} = Just $ fetchStart + fetchDuration
    getEnd FetchWait{..} = Just $ fetchWaitStart + fetchWaitDuration
    getEnd _ = Nothing
    minStartTime = minimum $ mapMaybe getStart validFetchStats
    endTime = maximum $ mapMaybe getEnd validFetchStats
    usPerDash = (endTime - minStartTime) `div` numDashes
    fetchSymbol FetchStats{} = '*'
    fetchSymbol FetchWait{} = '.'
    fetchSymbol _ = '?'
    fetchWasRunning :: FetchStats -> Timestamp -> Timestamp -> Bool
    fetchWasRunning fs@FetchStats{} t1 t2 =
      (fetchStart fs + fetchDuration fs) >= t1 && fetchStart fs < t2
    fetchWasRunning fw@FetchWait{} t1 t2 =
      (fetchWaitStart fw + fetchWaitDuration fw) >= t1 && fetchWaitStart fw < t2
    fetchWasRunning _ _ _ = False

type CallId = Int

-- | Maps data source name to the number of requests made in that round.
-- The map only contains entries for sources that made requests in that
-- round.
data FetchStats
    -- | Timing stats for a (batched) data fetch
  = FetchStats
    { fetchDataSource :: Text
    , fetchBatchSize :: {-# UNPACK #-} !Int
    , fetchStart :: {-# UNPACK #-} !Timestamp
    , fetchDuration :: {-# UNPACK #-} !Microseconds
    , fetchSpace :: {-# UNPACK #-} !Int64
    , fetchFailures :: {-# UNPACK #-} !Int
    , fetchIgnoredFailures :: {-# UNPACK #-} !Int
    , fetchBatchId :: {-# UNPACK #-} !Int
    , fetchIds :: [CallId]
    }

    -- | The stack trace of a call to 'dataFetch'.  These are collected
    -- only when profiling and reportLevel is 5 or greater.
  | FetchCall
    { fetchReq :: String
    , fetchStack :: [String]
    , fetchStatId :: {-# UNPACK #-} !CallId
    }
  | MemoCall
    { memoStatId :: {-# UNPACK #-} !CallId
    , memoSpace :: {-# UNPACK #-} !Int64
    }
  | FetchWait
    { fetchWaitReqs :: HashMap Text Int
       -- ^ What DataSources had requests that were being waited for
    , fetchWaitStart :: {-# UNPACK #-} !Timestamp
    , fetchWaitDuration :: {-# UNPACK #-} !Microseconds
    }
  deriving (Eq, Show)

-- | Pretty-print RoundStats.
ppFetchStats :: FetchStats -> String
ppFetchStats FetchStats{..} =
  printf "%s: %d fetches (%.2fms, %d bytes, %d failures)"
    (Text.unpack fetchDataSource) fetchBatchSize
    (fromIntegral fetchDuration / 1000 :: Double)  fetchSpace fetchFailures
ppFetchStats (FetchCall r ss _) = show r ++ '\n':show ss
ppFetchStats MemoCall{} = ""
ppFetchStats FetchWait{..}
  | HashMap.size fetchWaitReqs == 0 = msg "unexpected: Blocked on nothing"
  | HashMap.size fetchWaitReqs <= 2 =
    msg $ printf "Blocked on %s"
      (intercalate "," [printf "%s (%d reqs)" ds c
                       | (ds,c) <- HashMap.toList fetchWaitReqs])
  | otherwise = msg $ printf "Blocked on %d sources (%d reqs)"
                        (HashMap.size fetchWaitReqs)
                        (sum $ HashMap.elems fetchWaitReqs)
  where
    msg :: String -> String
    msg x = printf "%s (%.2fms)"
                x
                (fromIntegral fetchWaitDuration / 1000 :: Double)

-- | Aggregate stats merging FetchStats from the same dispatched batch into one.
aggregateFetchBatches :: ([FetchStats] -> a) -> Stats -> [a]
aggregateFetchBatches agg (Stats fetches) =
      map agg $
      groupBy ((==) `on` fetchBatchId) $
      sortOn (Down . fetchBatchId)
      [f | f@FetchStats{} <- fetches]

instance ToJSON FetchStats where
  toJSON FetchStats{..} = object
    [ "type" .= ("FetchStats" :: Text)
    , "datasource" .= fetchDataSource
    , "fetches" .= fetchBatchSize
    , "start" .= fetchStart
    , "duration" .= fetchDuration
    , "allocation" .= fetchSpace
    , "failures" .= fetchFailures
    , "ignoredFailures" .= fetchIgnoredFailures
    , "batchid" .= fetchBatchId
    , "fetchids" .= fetchIds
    ]
  toJSON (FetchCall req strs fid) = object
    [ "type" .= ("FetchCall" :: Text)
    , "request" .= req
    , "stack" .= strs
    , "fetchid" .= fid
    ]
  toJSON (MemoCall cid allocs) = object
    [ "type" .= ("MemoCall" :: Text)
    , "callid" .= cid
    , "allocation" .= allocs
    ]
  toJSON FetchWait{..} = object
    [ "type" .= ("FetchWait" :: Text)
    , "duration" .= fetchWaitDuration
    ]

emptyStats :: Stats
emptyStats = Stats []

numFetches :: Stats -> Int
numFetches (Stats rs) = sum [ fetchBatchSize | FetchStats{..} <- rs ]


-- ---------------------------------------------------------------------------
-- Profiling

type ProfileLabel = Text
type AllocCount = Int64
type LabelHitCount = Int64
type ProfileKey = Int64

data ProfileFetch = ProfileFetch
  { profileFetchFetchId :: {-# UNPACK #-} !CallId
  , profileFetchMemoId ::  {-# UNPACK #-} !CallId
  , profileFetchWasCached :: !Bool
  }
  deriving (Show, Eq)

data ProfileMemo = ProfileMemo
  { profileMemoId :: {-# UNPACK #-} !CallId
  , profileMemoWasCached :: !Bool
  }
  deriving (Show, Eq)

data Profile = Profile
  { profile      :: HashMap ProfileKey ProfileData
     -- ^ Data per key (essentially per call stack)
  , profileTree :: HashMap (ProfileLabel, ProfileKey) ProfileKey
     -- ^ (label, parent) -> current. The exception is the root which will have
     -- ("MAIN", 0) -> 0
  , profileNextKey :: ProfileKey
     -- ^ Provides a unique key per callstack
  }

emptyProfile :: Profile
emptyProfile = Profile HashMap.empty (HashMap.singleton ("MAIN", 0) 0) 1

data ProfileData = ProfileData
  { profileAllocs :: {-# UNPACK #-} !AllocCount
     -- ^ allocations made by this label
  , profileFetches :: [ProfileFetch]
     -- ^ fetches made in this label
  , profileLabelHits :: {-# UNPACK #-} !LabelHitCount
     -- ^ number of hits at this label
  , profileMemos :: [ProfileMemo]
     -- ^ memo and a boolean representing if it was cached at the time
  }
  deriving Show

emptyProfileData :: ProfileData
emptyProfileData = ProfileData 0 [] 0 []
