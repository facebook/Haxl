-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file. An additional grant of patent rights can
-- be found in the PATENTS file.

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Most users should import "Haxl.Core" instead of importing this
-- module directly.
module Haxl.Core.Memo (
  memo,
  memoFingerprint,
  MemoFingerprintKey(..),
  memoize, memoize1, memoize2
) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Data.Text (Text)
import Data.Typeable
import Data.Hashable
import Data.Word

import GHC.Prim (Addr#)

import Haxl.Core.Monad

-- -----------------------------------------------------------------------------
-- A key type that can be used for memoizing computations by a Text key

-- | Memoize a computation using an arbitrary key.  The result will be
-- calculated once; the second and subsequent time it will be returned
-- immediately.  It is the caller's responsibility to ensure that for
-- every two calls @memo key haxl@, if they have the same @key@ then
-- they compute the same result.
memo
  :: (Typeable a, Typeable k, Hashable k, Eq k)
  => k -> GenHaxl u a -> GenHaxl u a
memo key = cachedComputation (MemoKey key)

{-# RULES "memo/Text"
  memo = memoText :: (Typeable a) => Text -> GenHaxl u a -> GenHaxl u a
 #-}

{-# NOINLINE memo #-}

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

memoText :: (Typeable a) => Text -> GenHaxl u a -> GenHaxl u a
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
  :: Typeable a => MemoFingerprintKey a -> GenHaxl u a -> GenHaxl u a
memoFingerprint key@(MemoFingerprintKey _ _ mnPtr nPtr) =
  withFingerprintLabel mnPtr nPtr . cachedComputation key

-- * Generic memoization machinery.

-- | Transform a Haxl computation into a memoized version of itself.
--
-- Given a Haxl computation, @memoize@ creates a version which stores its result
-- in a @MemoVar@ (which @memoize@ creates), and returns the stored result on
-- subsequent invocations. This permits the creation of local memos, whose
-- lifetimes are scoped to the current function, rather than the entire request.
memoize :: GenHaxl u a -> GenHaxl u a
memoize a = newMemoWith a >>= runMemo

-- | Transform a 1-argument function returning a Haxl computation into a
-- memoized version of itself.
--
-- Given a function @f@ of type @a -> GenHaxl u b@, @memoize1@ creates a version
-- which memoizes the results of @f@ in a table keyed by its argument, and
-- returns stored results on subsequent invocations with the same argument.
--
-- e.g.:
--
-- allFriends :: [Int] -> GenHaxl u [Int]
-- allFriends ids = do
--   memoizedFriendsOf <- memoize1 friendsOf
--   concat <$> mapM memoizeFriendsOf ids
--
-- The above implementation will not invoke the underlying @friendsOf@
-- repeatedly for duplicate values in @ids@.
memoize1 :: (Eq a, Hashable a)
         => (a -> GenHaxl u b)
         -> GenHaxl u (a -> GenHaxl u b)
memoize1 f = runMemo1 <$> newMemoWith1 f

-- | Transform a 2-argument function returning a Haxl computation, into a
-- memoized version of itself.
--
-- The 2-ary version of @memoize1@, see its documentation for details.
memoize2 :: (Eq a, Hashable a, Eq b, Hashable b)
         => (a -> b -> GenHaxl u c)
         -> GenHaxl u (a -> b -> GenHaxl u c)
memoize2 f = runMemo2 <$> newMemoWith2 f
