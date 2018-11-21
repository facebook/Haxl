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
import Data.Text (Text)
import Data.Typeable
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import GHC.Exts
import qualified Data.Text as Text

import Haxl.Core.DataSource
import Haxl.Core.Flags
import Haxl.Core.Stats
import Haxl.Core.Monad

-- -----------------------------------------------------------------------------
-- Profiling

-- | Label a computation so profiling data is attributed to the label.
withLabel :: ProfileLabel -> GenHaxl u a -> GenHaxl u a
withLabel l (GenHaxl m) = GenHaxl $ \env ->
  if report (flags env) < 4
     then m env
     else collectProfileData l m env

-- | Label a computation so profiling data is attributed to the label.
-- Intended only for internal use by 'memoFingerprint'.
withFingerprintLabel :: Addr# -> Addr# -> GenHaxl u a -> GenHaxl u a
withFingerprintLabel mnPtr nPtr (GenHaxl m) = GenHaxl $ \env ->
  if report (flags env) < 4
     then m env
     else collectProfileData
            (Text.unpackCString# mnPtr <> "." <> Text.unpackCString# nPtr)
            m env

-- | Collect profiling data and attribute it to given label.
collectProfileData
  :: ProfileLabel
  -> (Env u -> IO (Result u a))
  -> Env u
  -> IO (Result u a)
collectProfileData l m env = do
   a0 <- getAllocationCounter
   r <- m env{profLabel=l} -- what if it throws?
   a1 <- getAllocationCounter
   modifyProfileData env l (a0 - a1)
   -- So we do not count the allocation overhead of modifyProfileData
   setAllocationCounter a1
   case r of
     Done a -> return (Done a)
     Throw e -> return (Throw e)
     Blocked ivar k -> return (Blocked ivar (Cont (withLabel l (toHaxl k))))
{-# INLINE collectProfileData #-}

modifyProfileData :: Env u -> ProfileLabel -> AllocCount -> IO ()
modifyProfileData env label allocs =
  modifyIORef' (profRef env) $ \ p ->
    p { profile =
          HashMap.insertWith updEntry label newEntry .
          HashMap.insertWith updCaller caller newCaller $
          profile p }
  where caller = profLabel env
        newEntry =
          emptyProfileData
            { profileAllocs = allocs
            , profileDeps = HashSet.singleton caller }
        updEntry _ old =
          old { profileAllocs = profileAllocs old + allocs
              , profileDeps = HashSet.insert caller (profileDeps old) }
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
  :: (Env u -> IO (Result u a))
  -> Env u
  -> IO (Result u a)
profileCont m env = do
  a0 <- getAllocationCounter
  r <- m env
  a1 <- getAllocationCounter
  let
    allocs = a0 - a1
    newEntry = emptyProfileData { profileAllocs = allocs }
    updEntry _ old = old { profileAllocs = profileAllocs old + allocs }
  modifyIORef' (profRef env) $ \ p ->
    p { profile =
         HashMap.insertWith updEntry (profLabel env) newEntry $
         profile p }
  -- So we do not count the allocation overhead of modifyProfileData
  setAllocationCounter a1
  return r
{-# INLINE profileCont #-}


incrementMemoHitCounterFor :: ProfileLabel -> Profile -> Profile
incrementMemoHitCounterFor lbl p =
  p { profile = HashMap.adjust incrementMemoHitCounter lbl (profile p) }

incrementMemoHitCounter :: ProfileData -> ProfileData
incrementMemoHitCounter pd = pd { profileMemoHits = succ (profileMemoHits pd) }

{-# NOINLINE addProfileFetch #-}
addProfileFetch
  :: forall r u a . (DataSourceName r, Eq (r a), Hashable (r a), Typeable (r a))
  => Env u -> r a -> IO ()
addProfileFetch env _req = do
  c <- getAllocationCounter
  modifyIORef' (profRef env) $ \ p ->
    let
      dsName :: Text
      dsName = dataSourceName (Proxy :: Proxy r)

      upd :: ProfileData -> ProfileData
      upd d = d { profileFetches =
        HashMap.insertWith (+) dsName 1 (profileFetches d) }

    in p { profile = HashMap.adjust upd (profLabel env) (profile p) }
  -- So we do not count the allocation overhead of addProfileFetch
  setAllocationCounter c
