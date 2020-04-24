-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Memoization support. This module is provided for access to Haxl
-- internals only; most users should import "Haxl.Core" instead.
--
module Haxl.Core.Memo
  (
    -- * Basic memoization
    cachedComputation
  , preCacheComputation

    -- * High-level memoization
  , memo
  , memoFingerprint
  , MemoFingerprintKey(..)
  , memoize, memoize1, memoize2
  , memoUnique

    -- * Local memoization
  , MemoVar
  , newMemo
  , newMemoWith
  , prepareMemo
  , runMemo
  ) where

import Control.Exception as Exception hiding (throw)
import Data.IORef
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import Data.Typeable
import Data.Hashable
import Data.Int
import Data.Word

import GHC.Prim (Addr#)

import Haxl.Core.Exception
import Haxl.Core.DataCache as DataCache
import Haxl.Core.Flags
import Haxl.Core.Monad
import Haxl.Core.Stats
import Haxl.Core.Profile
import Haxl.Core.Util (trace_)

-- -----------------------------------------------------------------------------
-- Memoization

-- | 'cachedComputation' memoizes a Haxl computation.  The key is a
-- request.
--
-- /Note:/ These cached computations will /not/ be included in the output
-- of 'dumpCacheAsHaskell'.
--
cachedComputation
   :: forall req u w a.
      ( Eq (req a)
      , Hashable (req a)
      , Typeable (req a))
   => req a -> GenHaxl u w a -> GenHaxl u w a

cachedComputation req haxl = GenHaxl $ \env@Env{..} -> do
  mbRes <- DataCache.lookup req memoCache
  case mbRes of
    Just (DataCacheItem ivar k) -> do
      ifProfiling flags $ do
        incrementMemoHitCounterFor env k True
      unHaxl (getIVarWithWrites ivar) env
    Nothing -> do
      ivar <- newIVar
      k <- nextCallId env
      -- no need to incremenetMemoHitCounter as execMemo will do it
      DataCache.insertNotShowable req (DataCacheItem ivar k) memoCache
      execMemoNowProfiled env haxl ivar k


-- | Like 'cachedComputation', but fails if the cache is already
-- populated.
--
-- Memoization can be (ab)used to "mock" a cached computation, by
-- pre-populating the cache with an alternative implementation. In
-- that case we don't want the operation to populate the cache to
-- silently succeed if the cache is already populated.
--
preCacheComputation
  :: forall req u w a.
     ( Eq (req a)
     , Hashable (req a)
     , Typeable (req a))
  => req a -> GenHaxl u w a -> GenHaxl u w a
preCacheComputation req haxl = GenHaxl $ \env@Env{..} -> do
  mbRes <- DataCache.lookup req memoCache
  case mbRes of
    Just _ -> return $ Throw $ toException $ InvalidParameter
      "preCacheComputation: key is already cached"
    Nothing -> do
      ivar <- newIVar
      k <- nextCallId env
      DataCache.insertNotShowable req (DataCacheItem ivar k) memoCache
      execMemoNowProfiled env haxl ivar k

-- -----------------------------------------------------------------------------
-- Memoization

newtype MemoVar u w a = MemoVar (IORef (MemoStatus u w a))

data MemoStatus u w a
  = MemoEmpty
  | MemoReady (GenHaxl u w a)
  | MemoRun {-# UNPACK #-} !(IVar u w a)

-- | Create a new @MemoVar@ for storing a memoized computation. The created
-- @MemoVar@ is initially empty, not tied to any specific computation. Running
-- this memo (with @runMemo@) without preparing it first (with @prepareMemo@)
-- will result in an exception.
newMemo :: GenHaxl u w (MemoVar u w a)
newMemo = unsafeLiftIO $ MemoVar <$> newIORef MemoEmpty

-- | Store a computation within a supplied @MemoVar@. Any memo stored within the
-- @MemoVar@ already (regardless of completion) will be discarded, in favor of
-- the supplied computation. A @MemoVar@ must be prepared before it is run.
prepareMemo :: MemoVar u w a -> GenHaxl u w a -> GenHaxl u w ()
prepareMemo (MemoVar memoRef) memoCmp
  = unsafeLiftIO $ writeIORef memoRef (MemoReady memoCmp)

-- | Convenience function, combines @newMemo@ and @prepareMemo@.
newMemoWith :: GenHaxl u w a -> GenHaxl u w (MemoVar u w a)
newMemoWith memoCmp = do
  memoVar <- newMemo
  prepareMemo memoVar memoCmp
  return memoVar

-- | Continue the memoized computation within a given @MemoVar@.
-- Notes:
--
--   1. If the memo contains a complete result, return that result.
--   2. If the memo contains an in-progress computation, continue it as far as
--      possible for this round.
--   3. If the memo is empty (it was not prepared), throw an error.
--
-- For example, to memoize the computation @one@ given by:
--
-- > one :: Haxl Int
-- > one = return 1
--
-- use:
--
-- > do
-- >   oneMemo <- newMemoWith one
-- >   let memoizedOne = runMemo aMemo one
-- >   oneResult <- memoizedOne
--
-- To memoize mutually dependent computations such as in:
--
-- > h :: Haxl Int
-- > h = do
-- >   a <- f
-- >   b <- g
-- >   return (a + b)
-- >  where
-- >   f = return 42
-- >   g = succ <$> f
--
-- without needing to reorder them, use:
--
-- > h :: Haxl Int
-- > h = do
-- >   fMemoRef <- newMemo
-- >   gMemoRef <- newMemo
-- >
-- >   let f = runMemo fMemoRef
-- >       g = runMemo gMemoRef
-- >
-- >   prepareMemo fMemoRef $ return 42
-- >   prepareMemo gMemoRef $ succ <$> f
-- >
-- >   a <- f
-- >   b <- g
-- >   return (a + b)
--
runMemo :: MemoVar u w a -> GenHaxl u w a
runMemo (MemoVar memoRef) = GenHaxl $ \env -> do
  stored <- readIORef memoRef
  case stored of
    -- Memo was not prepared first; throw an exception.
    MemoEmpty -> trace_ "MemoEmpty " $
      raise $ CriticalError "Attempting to run empty memo."
    -- Memo has been prepared but not run yet
    MemoReady cont -> trace_ "MemoReady" $ do
      ivar <- newIVar
      writeIORef memoRef (MemoRun ivar)
      execMemoNow env cont ivar
    -- The memo has already been run, get (or wait for) for the result
    MemoRun ivar -> trace_ "MemoRun" $ unHaxl (getIVarWithWrites ivar) env

execMemoNowProfiled
  :: Env u w
  -> GenHaxl u w a
  -> IVar u w a
  -> CallId
  -> IO (Result u w a)
execMemoNowProfiled envOuter cont ivar cid = if report (flags envOuter) < 4
  then execMemoNow envOuter cont ivar
  else do
    incrementMemoHitCounterFor envOuter cid False
    unHaxl
      (collectMemoData 0 $ GenHaxl $ \e -> execMemoNow e cont ivar)
      envOuter
  where
    addStats :: Env u w -> Int64 -> IO ()
    addStats env acc = modifyIORef' (statsRef env) $ \(Stats s) ->
      Stats (MemoCall cid acc : s)
    collectMemoData :: Int64 -> GenHaxl u w a -> GenHaxl u w a
    collectMemoData acc f = GenHaxl $ \env -> do
      a0 <- getAllocationCounter
      r <- unHaxl f env{memoKey=cid}
      a1 <- getAllocationCounter
      let newTotal = acc + (a0 - a1)
      ret <- case r of
        Done a -> do addStats env newTotal; return (Done a)
        Throw e -> do addStats env newTotal; return (Throw e)
        Blocked ivar k ->
          return (Blocked ivar (Cont (collectMemoData newTotal (toHaxl k))))
      setAllocationCounter a1
      return ret

execMemoNow :: Env u w -> GenHaxl u w a -> IVar u w a -> IO (Result u w a)
execMemoNow env cont ivar = do
  wlogs <- newIORef NilWrites
  let
    !ienv = imperative env { writeLogsRef = wlogs }
    -- don't speculate under memoized things
    -- also we won't an env with empty writes, so we can memoize the extra
    -- writes done as part of 'cont'
  r <- Exception.try $ unHaxl cont ienv

  case r of
    Left e -> trace_ ("execMemoNow: Left " ++ show e) $ do
      rethrowAsyncExceptions e
      putIVar ivar (ThrowIO e) env
      throwIO e
    Right (Done a) -> trace_ "execMemoNow: Done" $ do
      wt <- readIORef wlogs
      putIVar ivar (Ok a wt) env
      mbModifyWLRef wt (writeLogsRef env)
      return (Done a)
    Right (Throw ex) -> trace_ ("execMemoNow: Throw" ++ show ex) $ do
      wt <- readIORef wlogs
      putIVar ivar (ThrowHaxl ex wt) env
      mbModifyWLRef wt (writeLogsRef env)
      return (Throw ex)
    Right (Blocked ivar' cont) -> trace_ "execMemoNow: Blocked" $ do
      -- We "block" this memoized computation in the new environment 'ienv', so
      -- that when it finishes, we can store all the write logs from the env
      -- in the IVar.
      addJob ienv (toHaxl cont) ivar ivar'
      -- Now we call @getIVarWithWrites@ to populate the writes in the original
      -- environment 'env'.
      return (Blocked ivar (Cont (getIVarWithWrites ivar)))

-- -----------------------------------------------------------------------------
-- 1-ary and 2-ary memo functions

newtype MemoVar1 u w a b = MemoVar1 (IORef (MemoStatus1 u w a b))
newtype MemoVar2 u w a b c = MemoVar2 (IORef (MemoStatus2 u w a b c))

data MemoStatus1 u w a b
  = MemoEmpty1
  | MemoTbl1 (a -> GenHaxl u w b) (HashMap.HashMap a (MemoVar u w b))

data MemoStatus2 u w a b c
  = MemoEmpty2
  | MemoTbl2
      (a -> b -> GenHaxl u w c)
      (HashMap.HashMap a (HashMap.HashMap b (MemoVar u w c)))

newMemo1 :: GenHaxl u w (MemoVar1 u w a b)
newMemo1 = unsafeLiftIO $ MemoVar1 <$> newIORef MemoEmpty1

newMemoWith1 :: (a -> GenHaxl u w b) -> GenHaxl u w (MemoVar1 u w a b)
newMemoWith1 f = newMemo1 >>= \r -> prepareMemo1 r f >> return r

prepareMemo1 :: MemoVar1 u w a b -> (a -> GenHaxl u w b) -> GenHaxl u w ()
prepareMemo1 (MemoVar1 r) f
  = unsafeLiftIO $ writeIORef r (MemoTbl1 f HashMap.empty)

runMemo1 :: (Eq a, Hashable a) => MemoVar1 u w a b -> a -> GenHaxl u w b
runMemo1 (MemoVar1 r) k = unsafeLiftIO (readIORef r) >>= \case
  MemoEmpty1 -> throw $ CriticalError "Attempting to run empty memo."
  MemoTbl1 f h -> case HashMap.lookup k h of
    Nothing -> do
      x <- newMemoWith (f k)
      unsafeLiftIO $ writeIORef r (MemoTbl1 f (HashMap.insert k x h))
      runMemo x
    Just v -> runMemo v

newMemo2 :: GenHaxl u w (MemoVar2 u w a b c)
newMemo2 = unsafeLiftIO $ MemoVar2 <$> newIORef MemoEmpty2

newMemoWith2 :: (a -> b -> GenHaxl u w c) -> GenHaxl u w (MemoVar2 u w a b c)
newMemoWith2 f = newMemo2 >>= \r -> prepareMemo2 r f >> return r

prepareMemo2 :: MemoVar2 u w a b c -> (a -> b -> GenHaxl u w c) -> GenHaxl u w ()
prepareMemo2 (MemoVar2 r) f
  = unsafeLiftIO $ writeIORef r (MemoTbl2 f HashMap.empty)

runMemo2 :: (Eq a, Hashable a, Eq b, Hashable b)
         => MemoVar2 u w a b c
         -> a -> b -> GenHaxl u w c
runMemo2 (MemoVar2 r) k1 k2 = unsafeLiftIO (readIORef r) >>= \case
  MemoEmpty2 -> throw $ CriticalError "Attempting to run empty memo."
  MemoTbl2 f h1 -> case HashMap.lookup k1 h1 of
    Nothing -> do
      v <- newMemoWith (f k1 k2)
      unsafeLiftIO $ writeIORef r
        (MemoTbl2 f (HashMap.insert k1 (HashMap.singleton k2 v) h1))
      runMemo v
    Just h2 -> case HashMap.lookup k2 h2 of
      Nothing -> do
        v <- newMemoWith (f k1 k2)
        unsafeLiftIO $ writeIORef r
          (MemoTbl2 f (HashMap.insert k1 (HashMap.insert k2 v h2) h1))
        runMemo v
      Just v -> runMemo v

-- -----------------------------------------------------------------------------
-- A key type that can be used for memoizing computations by a Text key

-- | Memoize a computation using an arbitrary key.  The result will be
-- calculated once; the second and subsequent time it will be returned
-- immediately.  It is the caller's responsibility to ensure that for
-- every two calls @memo key haxl@, if they have the same @key@ then
-- they compute the same result.
memo
  :: (Typeable a, Typeable k, Hashable k, Eq k)
  => k -> GenHaxl u w a -> GenHaxl u w a
memo key = cachedComputation (MemoKey key)

{-# RULES
"memo/Text" memo = memoText :: (Typeable a) =>
            Text -> GenHaxl u w a -> GenHaxl u w a
 #-}

{-# NOINLINE memo #-}

-- | Memoize a computation using its location and a Fingerprint. This ensures
-- uniqueness across computations.
memoUnique
  :: (Typeable a, Typeable k, Hashable k, Eq k)
  => MemoFingerprintKey a -> Text -> k -> GenHaxl u w a -> GenHaxl u w a
memoUnique fp label key = withLabel label . memo (fp, key)

{-# NOINLINE memoUnique #-}

data MemoKey k a where
  MemoKey :: (Typeable k, Hashable k, Eq k) => k -> MemoKey k a
  deriving Typeable

deriving instance Eq (MemoKey k a)

instance Hashable (MemoKey k a) where
  hashWithSalt s (MemoKey t) = hashWithSalt s t

-- An optimised memo key for Text keys.  This is used automatically
-- when the key is Text, due to the RULES pragma above.

data MemoTextKey a where
  MemoText :: Text -> MemoTextKey a
  deriving Typeable

deriving instance Eq (MemoTextKey a)

instance Hashable (MemoTextKey a) where
  hashWithSalt s (MemoText t) = hashWithSalt s t

memoText :: (Typeable a) => Text -> GenHaxl u w a -> GenHaxl u w a
memoText key = withLabel key . cachedComputation (MemoText key)

-- | A memo key derived from a 128-bit MD5 hash.  Do not use this directly,
-- it is for use by automatically-generated memoization.
data MemoFingerprintKey a where
  MemoFingerprintKey
    :: {-# UNPACK #-} !Word64
    -> {-# UNPACK #-} !Word64
    -> Addr# -> Addr#
    -> MemoFingerprintKey a
  deriving Typeable

deriving instance Eq (MemoFingerprintKey a)

instance Hashable (MemoFingerprintKey a) where
  hashWithSalt s (MemoFingerprintKey x _ _ _) =
    hashWithSalt s (fromIntegral x :: Int)

-- This is optimised for cheap call sites: when we have a call
--
--   memoFingerprint (MemoFingerprintKey 1234 5678 "module"# "name"#) e
--
-- then the MemoFingerprintKey constructor will be statically
-- allocated (with two 64-bit fields and pointers to cstrings for the names),
-- and shared by all calls to memo. So the memo call will not allocate,
-- unlike memoText.
--
{-# NOINLINE memoFingerprint #-}
memoFingerprint
  :: Typeable a => MemoFingerprintKey a -> GenHaxl u w a -> GenHaxl u w a
memoFingerprint key@(MemoFingerprintKey _ _ mnPtr nPtr) =
  withFingerprintLabel mnPtr nPtr . cachedComputation key

-- * Generic memoization machinery.

-- | Transform a Haxl computation into a memoized version of itself.
--
-- Given a Haxl computation, @memoize@ creates a version which stores its result
-- in a @MemoVar@ (which @memoize@ creates), and returns the stored result on
-- subsequent invocations. This permits the creation of local memos, whose
-- lifetimes are scoped to the current function, rather than the entire request.
memoize :: GenHaxl u w a -> GenHaxl u w (GenHaxl u w a)
memoize a = runMemo <$> newMemoWith a

-- | Transform a 1-argument function returning a Haxl computation into a
-- memoized version of itself.
--
-- Given a function @f@ of type @a -> GenHaxl u w b@, @memoize1@ creates a version
-- which memoizes the results of @f@ in a table keyed by its argument, and
-- returns stored results on subsequent invocations with the same argument.
--
-- e.g.:
--
-- > allFriends :: [Int] -> GenHaxl u w [Int]
-- > allFriends ids = do
-- >   memoizedFriendsOf <- memoize1 friendsOf
-- >   concat <$> mapM memoizeFriendsOf ids
--
-- The above implementation will not invoke the underlying @friendsOf@
-- repeatedly for duplicate values in @ids@.
memoize1 :: (Eq a, Hashable a)
         => (a -> GenHaxl u w b)
         -> GenHaxl u w (a -> GenHaxl u w b)
memoize1 f = runMemo1 <$> newMemoWith1 f

-- | Transform a 2-argument function returning a Haxl computation, into a
-- memoized version of itself.
--
-- The 2-ary version of @memoize1@, see its documentation for details.
memoize2 :: (Eq a, Hashable a, Eq b, Hashable b)
         => (a -> b -> GenHaxl u w c)
         -> GenHaxl u w (a -> b -> GenHaxl u w c)
memoize2 f = runMemo2 <$> newMemoWith2 f
