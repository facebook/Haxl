{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{- |
Module: Haxl.Core.FiniteCache
Description: A bounded cache for a specific type of request

The cache is built upon a priority queue from the psqueues package.

HashPSQ from that package uses Ord for resolving hash collisions, so
we cannot use it without imposing an Ord instance for the requests.

Instead, each hash bucket is a linked list, sorted by
priority (highest first). Because of monotony (a newly inserted value
always has highest prioroty), this is easy to enforce.

The design of the cache loosely follows the nice blog post by Jasper Van
der Jeugt, http://jaspervdj.be/posts/2015-02-24-lru-cache.html.
-}

module Haxl.Core.FiniteCache
       ( FiniteCache
       , singleton
       , mergeSingleton
       , lookup
       , toList)
       where

import           Control.Exception (assert)
import           Data.Hashable
import           Data.Int          (Int64)
import qualified Data.IntPSQ       as IntPSQ
import           Data.Maybe
import           Prelude           hiding (lookup)

type Priority = Int64

-- | Strict tuple of priority, key, and value.
data PV k v = PV !Priority !k !v
              deriving Show

-- | A hash bucket. Values with the same hash are stored in a list,
-- where the value of highest priority is always at the front.
data Bucket k v = Bucket ![PV k v]
                  deriving Show

data FiniteCache k v =
  FiniteCache { cCapacity :: !(Maybe Int)
                -- ^ Capacity of the subcache. Nothing means unbounded
              , cSize     :: !Int
                -- ^ Current size
              , cTick     :: !Priority
                -- ^ Priority of the next value inserted
              , cQueue    :: !(IntPSQ.IntPSQ Priority (Bucket k v))
                -- ^ priority queue storing the actual values.
                --
                --   The priority of a bucket is the priority of its
                --   highest-priority value.
              } deriving Show

-- | Initialisation of an empty 'FiniteCache'.
empty :: Maybe Int -> FiniteCache k v
empty capacity
  | isJust capacity && ((<0) . fromJust) capacity =
    error "FiniteCache.empty: negative capacity."
  | otherwise =
    FiniteCache { cCapacity = capacity
                , cSize = 0
                , cTick = 0
                , cQueue = IntPSQ.empty
                }

-- | A 'FiniteCache' with only one element.
singleton :: (Hashable k, Eq k) => Maybe Int -> k -> v -> FiniteCache k v
singleton capacity k v = insert k v $ empty capacity

-- | This is an insert, where the element to be inserted is given as a singleton.
--
-- This is needed in DataCache in order to insert an element while
-- only searching through the outer HashMap once, via insertWith.
mergeSingleton :: (Hashable k, Eq k)
                  => FiniteCache k v
                  -- ^ contains a single element ...
                  -> FiniteCache k v
                  -- ^ ... which is inserted in this cache.
                  -> FiniteCache k v
mergeSingleton new old =
-- It is a bit awkward that the order of the arguments has to match
-- with the order in which HashMap.insertWith expects, but I don't
-- know a better solution than this assert.
  assert ((IntPSQ.size . cQueue) new == 1) $
    let (Just (_, _, Bucket [PV _ k v])) = IntPSQ.findMin (cQueue new)
    in insert k v old

-- | Delete the value with minimal priority.
deleteMin :: FiniteCache k v -> FiniteCache k v
deleteMin sc@FiniteCache {..}
  | IntPSQ.null cQueue = sc
  | otherwise =
    let (_, cQueue') = IntPSQ.alterMin f cQueue
    in FiniteCache { cCapacity = cCapacity
                , cSize = cSize - 1
                , cTick = cTick
                , cQueue = cQueue'
                }
  where
    f Nothing = error "FiniteCache.deleteMin: non-empty cache without minimal element"
    -- if the bucket contains only one value, it is deleted
    f (Just (_, _, Bucket [_])) = (Nothing, Nothing)
    -- if there are more values, the last one is discarded.
    -- Note that this dos not change the priority of the bucket.
    f (Just (k, p, Bucket pvs)) = (Nothing, Just (k, p, Bucket (init pvs)))

-- | Make sure the cache does not exceed its capacity.
trim :: FiniteCache k v -> FiniteCache k v
trim sc
  -- In case of a capacity overflow, just restart with an empty
  -- cache.  It's the simplest thing to do, and an overflow
  -- should be a rare event.
  | cTick sc == maxBound = empty (cCapacity sc)
  -- Don't trim unbounded caches.
  | isNothing $ cCapacity sc = sc
  -- Remove element of least priority when full
  | cSize sc > fromJust (cCapacity sc) = deleteMin sc
  | otherwise = sc

insert :: (Hashable k, Eq k)
          => k
          -> v
          -> FiniteCache k v
          -> FiniteCache k v
insert !k !v FiniteCache {..} =
  let (cSize', cQueue') = IntPSQ.alter f (hash k) cQueue
  in trim FiniteCache { cCapacity = cCapacity
                      , cSize = cSize'
                      , cTick = cTick + 1
                      , cQueue = cQueue'
                      }
  where
    -- no bucket for that hash yet, create one
    pv = PV cTick k v
    f Nothing = (cSize + 1, Just (cTick, Bucket [pv]))
    f (Just (_, Bucket b)) =
      case break (\ (PV _ k' _) -> k' == k ) b of
        -- request is not yet cached
        (xs,[]) -> (cSize + 1, Just (cTick, Bucket (pv:xs)))
        -- request had already been cached, update it
        (xs, xs') -> (cSize, Just (cTick, Bucket (pv:(xs++tail xs'))))

lookup :: (Hashable k, Eq k)
          => k
          -> FiniteCache k v
          -> Maybe (v, FiniteCache k v)
lookup k FiniteCache {..} =
  case IntPSQ.alter f (hash k) cQueue
  of (Just v, cQueue') -> Just (v, FiniteCache { cCapacity = cCapacity
                                                , cSize = cSize
                                                , cQueue = cQueue'
                                                , cTick = cTick + 1})
     (Nothing, _) -> Nothing
  where
    f Nothing = (Nothing, Nothing)
    f (Just (p, Bucket b)) =
      case break (\ (PV _ k' _) -> k' == k ) b of
          (_, []) ->  (Nothing, Just (p, Bucket b))
          (xs, PV _ k' v:xs') ->
            ( Just v
            , Just (cTick, Bucket $ PV cTick k' v:(xs ++ xs')))

toList :: FiniteCache k v -> [(k, v)]
toList FiniteCache { cQueue = cq } = IntPSQ.fold' f [] cq
  where f _ _ (Bucket b) acc = map (\ (PV _ k v) -> (k, v) ) b ++ acc
