-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Implementation of lightweight profiling.  Most users should
-- import "Haxl.Core" instead.
--
module Haxl.Core.Profile
  ( withLabel
  , withFingerprintLabel
  , addProfileFetch
  , incrementMemoHitCounterFor
  , collectProfileData
  , profileCont
  ) where

import Data.IORef
import Data.Hashable
#if __GLASGOW_HASKELL__ < 804
import Data.Monoid
#endif
import Data.Typeable
import qualified Data.HashMap.Strict as HashMap
import GHC.Exts
import qualified Data.Text as Text
import Haxl.Core.DataSource
import Haxl.Core.Flags
import Haxl.Core.Stats
import Haxl.Core.Monad

-- -----------------------------------------------------------------------------
-- Profiling

-- | Label a computation so profiling data is attributed to the label.
withLabel :: ProfileLabel -> GenHaxl u w a -> GenHaxl u w a
withLabel l (GenHaxl m) = GenHaxl $ \env ->
  if report (flags env) < 4
     then m env
     else collectProfileData l m env

-- | Label a computation so profiling data is attributed to the label.
-- Intended only for internal use by 'memoFingerprint'.
withFingerprintLabel :: Addr# -> Addr# -> GenHaxl u w a -> GenHaxl u w a
withFingerprintLabel mnPtr nPtr (GenHaxl m) = GenHaxl $ \env ->
  if report (flags env) < 4
     then m env
     else collectProfileData
            (Text.unpackCString# mnPtr <> "." <> Text.unpackCString# nPtr)
            m env

-- | Collect profiling data and attribute it to given label.
collectProfileData
  :: ProfileLabel
  -> (Env u w -> IO (Result u w a))
  -> Env u w
  -> IO (Result u w a)
collectProfileData l m env = do
  let (ProfileCurrent prevProfKey prevProfLabel) = profCurrent env
  if prevProfLabel == l
  then
    -- do not add a new label if we are recursing
    m env
  else do
    key <- atomicModifyIORef' (profRef env) $ \p ->
      case HashMap.lookup (l, prevProfKey) (profileTree p) of
        Just k -> (p, k)
        Nothing -> (p
          { profileTree = HashMap.insert
            (l, prevProfKey)
            (profileNextKey p)
            (profileTree p)
          , profileNextKey = profileNextKey p + 1 }, profileNextKey p)
    runProfileData l key m False env
{-# INLINE collectProfileData #-}

runProfileData
  :: ProfileLabel
  -> ProfileKey
  -> (Env u w -> IO (Result u w a))
  -> Bool
  -> Env u w
  -> IO (Result u w a)
runProfileData l key m isCont env = do
  a0 <- getAllocationCounter
  let
    nextCurrent = ProfileCurrent
                  { profCurrentKey = key
                  , profCurrentLabel = l }
    caller = profCurrentKey (profCurrent env)

  r <- m env{profCurrent=nextCurrent} -- what if it throws?
  a1 <- getAllocationCounter

  -- caller might not be the actual caller of this function
  -- for example MAIN may be continuing a function from the middle of the stack.
  -- But this is what we want as we need to account for allocations.
  -- So do not be tempted to pass through prevProfKey (from collectProfileData)
  -- which is the original caller
  modifyProfileData env key caller (a0 - a1) (if isCont then 0 else 1)

  -- So we do not count the allocation overhead of modifyProfileData
  setAllocationCounter a1
  case r of
    Done a -> return (Done a)
    Throw e -> return (Throw e)
    Blocked ivar k -> return (Blocked ivar (Cont $ runCont (toHaxl k)))
  where
    runCont (GenHaxl h) = GenHaxl $ runProfileData l key h True
{-# INLINE runProfileData #-}

modifyProfileData
  :: Env u w
  -> ProfileKey
  -> ProfileKey
  -> AllocCount
  -> LabelHitCount
  -> IO ()
modifyProfileData env key caller allocs labelIncrement = do
  modifyIORef' (profRef env) $ \ p ->
    p { profile =
          HashMap.insertWith updEntry key newEntry .
          HashMap.insertWith updCaller caller newCaller $
          profile p }
  where newEntry =
          emptyProfileData
            { profileAllocs = allocs
            , profileLabelHits = labelIncrement
            }
        updEntry _ old =
          old
            { profileAllocs = profileAllocs old + allocs
            , profileLabelHits = profileLabelHits old + labelIncrement
            }
        -- subtract allocs from caller, so they are not double counted
        -- we don't know the caller's caller, but it will get set on
        -- the way back out, so an empty hashset is fine for now
        newCaller =
          emptyProfileData { profileAllocs = -allocs }
        updCaller _ old =
          old { profileAllocs = profileAllocs old - allocs }


-- Like collectProfileData, but intended to be run from the scheduler.
--
-- * doesn't add a dependency (the original withLabel did this)
--
-- * doesn't subtract allocs from the caller (we're evaluating this
--   cont from the top level, so we don't need this)
--
-- * doesn't wrap a Blocked continuation in withLabel (the scheduler
--   will call profileCont the next time this cont runs)
--
profileCont
  :: (Env u w -> IO (Result u w a))
  -> Env u w
  -> IO (Result u w a)
profileCont m env = do
  a0 <- getAllocationCounter
  r <- m env
  a1 <- getAllocationCounter
  let
    allocs = a0 - a1
    newEntry = emptyProfileData { profileAllocs = allocs }
    updEntry _ old = old { profileAllocs = profileAllocs old + allocs }
    profKey = profCurrentKey (profCurrent env)
  modifyIORef' (profRef env) $ \ p ->
    p { profile =
         HashMap.insertWith updEntry profKey newEntry $
         profile p }
  -- So we do not count the allocation overhead of modifyProfileData
  setAllocationCounter a1
  return r
{-# INLINE profileCont #-}

incrementMemoHitCounterFor :: Env u w -> CallId -> Bool -> IO ()
incrementMemoHitCounterFor env callId wasCached = do
  modifyIORef' (profRef env) $ \p ->  p {
    profile = HashMap.insertWith
                upd
                (profCurrentKey $ profCurrent env)
                (emptyProfileData { profileMemos = [val] })
                (profile p)
    }
  where
    val = ProfileMemo callId wasCached
    upd _ old = old { profileMemos = val : profileMemos old }

{-# NOINLINE addProfileFetch #-}
addProfileFetch
  :: forall r u w a . (DataSourceName r, Eq (r a), Hashable (r a), Typeable (r a))
  => Env u w -> r a -> CallId -> Bool -> IO ()
addProfileFetch env _req cid wasCached = do
  c <- getAllocationCounter
  let (ProfileCurrent profKey _) = profCurrent env
  modifyIORef' (profRef env) $ \ p ->
    let
      val = ProfileFetch cid (memoKey env) wasCached
      upd _ old = old { profileFetches = val : profileFetches old }

    in p { profile =
           HashMap.insertWith
             upd
             profKey
             (emptyProfileData { profileFetches = [val] })
             (profile p)
         }
  -- So we do not count the allocation overhead of addProfileFetch
  setAllocationCounter c
