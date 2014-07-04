{-# LANGUAGE StandaloneDeriving, GADTs, DeriveDataTypeable #-}
module DataCacheTest (tests) where

import Haxl.Core.DataCache as DataCache
import Haxl.Core

import Control.Exception
import Data.Hashable
import Data.Traversable
import Data.Typeable
import Prelude hiding (mapM)
import Test.HUnit

data TestReq a where
   Req :: Int -> TestReq a -- polymorphic result
  deriving Typeable

deriving instance Eq (TestReq a)
deriving instance Show (TestReq a)

instance Hashable (TestReq a) where
  hashWithSalt salt (Req i) = hashWithSalt salt i


dcSoundnessTest :: Test
dcSoundnessTest = TestLabel "DataCache soundness" $ TestCase $ do
  m1 <- newResult 1
  m2 <- newResult "hello"
  let cache =
          DataCache.insert (Req 1 :: TestReq Int) m1 $
          DataCache.insert (Req 2 :: TestReq String) m2 $
          DataCache.empty

  -- "Req 1" has a result of type Int, so if we try to look it up
  -- with a result of type String, we should get Nothing, not a crash.
  r <- mapM takeResult $ DataCache.lookup (Req 1) cache
  assertBool "dcSoundness1" $
    case r :: Maybe (Either SomeException String) of
     Nothing -> True
     _something_else -> False

  r <- mapM takeResult $ DataCache.lookup (Req 1) cache
  assertBool "dcSoundness2" $
    case r :: Maybe (Either SomeException Int) of
     Just (Right 1) -> True
     _something_else -> False

  r <- mapM takeResult $ DataCache.lookup (Req 2) cache
  assertBool "dcSoundness3" $
    case r :: Maybe (Either SomeException String) of
      Just (Right "hello") -> True
      _something_else -> False

  r <- mapM takeResult $ DataCache.lookup (Req 2) cache
  assertBool "dcSoundness4" $
    case r :: Maybe (Either SomeException Int) of
      Nothing -> True
      _something_else -> False


dcStrictnessTest :: Test
dcStrictnessTest = TestLabel "DataCache strictness" $ TestCase $ do
  env <- initEnv stateEmpty ()
  r <- Control.Exception.try $ runHaxl env $
    cachedComputation (Req (error "BOOM")) $ return "OK"
  assertBool "dcStrictnessTest" $
    case r of
      Left (ErrorCall "BOOM") -> True
      _other -> False

-- tests :: Assertion
tests = TestList [dcSoundnessTest, dcStrictnessTest]
