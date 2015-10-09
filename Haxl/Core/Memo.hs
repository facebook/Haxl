-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file. An additional grant of patent rights can
-- be found in the PATENTS file.

{-# LANGUAGE DeriveDataTypeable, GADTs, OverloadedStrings,
    StandaloneDeriving #-}

-- | Most users should import "Haxl.Core" instead of importing this
-- module directly.
module Haxl.Core.Memo (memo, memoFingerprint, MemoFingerprintKey(..)) where

import Data.Text (Text)
import Data.Typeable
import Data.Hashable
import Data.Word

import Haxl.Core.Monad (GenHaxl, cachedComputation)

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
deriving instance Show (MemoTextKey a)

instance Hashable (MemoTextKey a) where
  hashWithSalt s (MemoText t) = hashWithSalt s t

memoText :: (Typeable a) => Text -> GenHaxl u a -> GenHaxl u a
memoText key = cachedComputation (MemoText key)

-- | A memo key derived from a 128-bit MD5 hash.  Do not use this directly,
-- it is for use by automatically-generated memoization.

data MemoFingerprintKey a where
  MemoFingerprintKey
    :: {-# UNPACK #-} !Word64
    -> {-# UNPACK #-} !Word64  -> MemoFingerprintKey a
  deriving Typeable

deriving instance Eq (MemoFingerprintKey a)
deriving instance Show (MemoFingerprintKey a)

instance Hashable (MemoFingerprintKey a) where
  hashWithSalt s (MemoFingerprintKey x _) =
    hashWithSalt s (fromIntegral x :: Int)

-- This is optimised for cheap call sites: when we have a call
--
--   memoFingerprint (MemoFingerprintKey 1234 5678) e
--
-- then the MemoFingerprintKey constructor will be statically
-- allocated (with two 64-bit fields), and shared by all calls to
-- memo.  So the memo call will not allocate, unlike memoText.
--
{-# NOINLINE memoFingerprint #-}
memoFingerprint
   :: (Show a, Typeable a) => MemoFingerprintKey a -> GenHaxl u a -> GenHaxl u a
memoFingerprint key = cachedComputation key
