-- Copyright (c) 2014, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file. An additional grant of patent rights can
-- be found in the PATENTS file.

{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Generic fetching infrastructure, used by 'Haxl.Core.Monad'.
module Haxl.Core.Fetch
  ( CacheResult(..)
  , cached
  , memoized
  , performFetches
  ) where

import Haxl.Core.DataCache as DataCache
import Haxl.Core.Env
import Haxl.Core.Exception
import Haxl.Core.RequestStore
import Haxl.Core.Show1
import Haxl.Core.StateStore
import Haxl.Core.Types
import Haxl.Core.Util

import Control.Exception
import Control.Monad
import Data.IORef
import Data.List
import Data.Time
import Text.Printf
import Data.Monoid
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

-- | Issues a batch of fetches in a 'RequestStore'. After
-- 'performFetches', all the requests in the 'RequestStore' are
-- complete, and all of the 'ResultVar's are full.
performFetches :: forall u. Env u -> RequestStore u -> IO ()
performFetches env reqs = do
  let f = flags env
      sref = statsRef env
      jobs = contents reqs

  t0 <- getCurrentTime

  let
    roundstats =
      [ (dataSourceName (getReq reqs), length reqs)
      | BlockedFetches reqs <- jobs ]
      where
      getReq :: [BlockedFetch r] -> r a
      getReq = undefined

  modifyIORef' sref $ \(Stats rounds) ->
     Stats (RoundStats (HashMap.fromList roundstats) : rounds)

  ifTrace f 1 $
    printf "Batch data fetch (%s)\n" $
       intercalate (", "::String) $
           map (\(name,num) -> printf "%d %s" num (Text.unpack name)) roundstats

  ifTrace f 3 $
    forM_ jobs $ \(BlockedFetches reqs) ->
      forM_ reqs $ \(BlockedFetch r _) -> putStrLn (show1 r)

  let
    applyFetch (BlockedFetches (reqs :: [BlockedFetch r])) =
      case stateGet (states env) of
        Nothing ->
          return (SyncFetch (mapM_ (setError (const e)) reqs))
          where req :: r a; req = undefined
                e = DataSourceError $
                      "data source not initialized: " <> dataSourceName req
        Just state ->
          return $ wrapFetch reqs $ fetch state f (userEnv env) reqs

  fetches <- mapM applyFetch jobs

  scheduleFetches fetches

  ifTrace f 1 $ do
    t1 <- getCurrentTime
    printf "Batch data fetch done (%.2fs)\n"
      (realToFrac (diffUTCTime t1 t0) :: Double)

-- Catch exceptions arising from the data source and stuff them into
-- the appropriate requests.  We don't want any exceptions propagating
-- directly from the data sources, because we want the exception to be
-- thrown by dataFetch instead.
--
wrapFetch :: [BlockedFetch req] -> PerformFetch -> PerformFetch
wrapFetch reqs fetch =
  case fetch of
    SyncFetch io -> SyncFetch (io `catch` handler)
    AsyncFetch fio -> AsyncFetch (\io -> fio io `catch` handler)
  where
    handler :: SomeException -> IO ()
    handler e = mapM_ (forceError e) reqs

    -- Set the exception even if the request already had a result.
    -- Otherwise we could be discarding an exception.
    forceError e (BlockedFetch _ rvar) = do
      void $ tryTakeResult rvar
      putResult rvar (except e)

-- | Start all the async fetches first, then perform the sync fetches before
-- getting the results of the async fetches.
scheduleFetches :: [PerformFetch] -> IO()
scheduleFetches fetches = async_fetches sync_fetches
 where
  async_fetches :: IO () -> IO ()
  async_fetches = compose [f | AsyncFetch f <- fetches]

  sync_fetches :: IO ()
  sync_fetches = sequence_ [io | SyncFetch io <- fetches]

-- | Possible responses when checking the cache.
data CacheResult a
  -- | The request hadn't been seen until now.
  = Uncached (ResultVar a)

  -- | The request has been seen before, but its result has not yet been
  -- fetched.
  | CachedNotFetched (ResultVar a)

  -- | The request has been seen before, and its result has already been
  -- fetched.
  | Cached (Either SomeException a)


-- | Checks the data cache for the result of a request.
cached :: (Request r a) => Env u -> r a -> IO (CacheResult a)
cached env = checkCache (flags env) (cacheRef env)

-- | Checks the memo cache for the result of a computation.
memoized :: (Request r a) => Env u -> r a -> IO (CacheResult a)
memoized env = checkCache (flags env) (memoRef env)

-- | Common guts of 'cached' and 'memoized'.
checkCache
  :: (Request r a)
  => Flags
  -> IORef DataCache
  -> r a
  -> IO (CacheResult a)

checkCache flags ref req = do
  cache <- readIORef ref
  let
    do_fetch = do
      rvar <- newEmptyResult
      writeIORef ref (DataCache.insert req rvar cache)
      return (Uncached rvar)
  case DataCache.lookup req cache of
    Nothing -> do_fetch
    Just rvar -> do
      mb <- tryReadResult rvar
      case mb of
        Nothing -> return (CachedNotFetched rvar)
        -- Use the cached result, even if it was an error.
        Just r -> do
          ifTrace flags 3 $ putStrLn $ case r of
            Left _ -> "Cached error: " ++ show req
            Right _ -> "Cached request: " ++ show req
          return (Cached r)
