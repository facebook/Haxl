-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE StandaloneDeriving, GADTs, DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module DataCacheTest (tests, newResult, takeResult) where

import Haxl.Core.DataCache as DataCache
import Haxl.Core.Monad
import Haxl.Core

import Control.Exception
import Data.Either
import Data.Hashable
import Data.Traversable
import Data.Typeable
import Prelude hiding (mapM)
import Test.HUnit
import Data.IORef
import Data.Text
import Unsafe.Coerce

data TestReq a where
   Req :: Int -> TestReq a -- polymorphic result
  deriving Typeable

deriving instance Eq (TestReq a)
deriving instance Show (TestReq a)

instance Hashable (TestReq a) where
  hashWithSalt salt (Req i) = hashWithSalt salt i

instance DataSource u TestReq where
  fetch = error "no fetch defined"

instance DataSourceName TestReq where
  dataSourceName _ = pack "TestReq"

instance StateKey TestReq where
  data State TestReq = TestReqState

instance ShowP TestReq where showp = show

data CacheableReq x where CacheableInt :: Int -> CacheableReq Int
  deriving Typeable
deriving instance Eq (CacheableReq x)
deriving instance Show (CacheableReq x)
instance Hashable (CacheableReq x) where
  hashWithSalt s (CacheableInt val) = hashWithSalt s (0::Int, val)


newResult :: a -> IO (IVar u w a)
newResult a = IVar <$> newIORef (IVarFull (Ok a NilWrites))

takeResult :: IVar u w a -> IO (ResultVal a w)
takeResult (IVar ref) = do
  e <- readIORef ref
  case e of
    IVarFull a -> return a
    _ -> error "takeResult"


dcSoundnessTest :: Test
dcSoundnessTest = TestLabel "DataCache soundness" $ TestCase $ do
  m1 <- newResult 1
  m2 <- newResult "hello"
  cache <- emptyDataCache
  DataCache.insert (Req 2 :: TestReq String) m2 cache
  DataCache.insert (Req 1 :: TestReq Int) m1 cache

  -- "Req 1" has a result of type Int, so if we try to look it up
  -- with a result of type String, we should get Nothing, not a crash.
  r <- mapM takeResult =<< DataCache.lookup (Req 1) cache
  assertBool "dcSoundness1" $
    case r :: Maybe (ResultVal String ()) of
     Nothing -> True
     _something_else -> False

  r <- mapM takeResult =<< DataCache.lookup (Req 1) cache
  assertBool "dcSoundness2" $
    case r :: Maybe (ResultVal Int ()) of
     Just (Ok 1 NilWrites) -> True
     _something_else -> False

  r <- mapM takeResult =<< DataCache.lookup (Req 2) cache
  assertBool "dcSoundness3" $
    case r :: Maybe (ResultVal String ()) of
      Just (Ok "hello" NilWrites) -> True
      _something_else -> False

  r <- mapM takeResult =<< DataCache.lookup (Req 2) cache
  assertBool "dcSoundness4" $
    case r :: Maybe (ResultVal Int ()) of
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

dcFallbackTest :: Test
dcFallbackTest = TestLabel "DataCache fallback" $ TestList
  [ TestLabel "Base" $ TestCase $ do
      env <- mkEnv
      (r,cached) <- runHaxl env (do
                           a <- dataFetch req
                           b <- cacheResult (CacheableInt 1234) (return 99999)
                           return (a,b))
      (Stats stats) <- readIORef (statsRef env)
      assertEqual "fallback still has stats" 1
        (Prelude.length [x | x@FetchStats{} <- stats])
      assertEqual "dcFallbackTest found" 1 r
      assertEqual "dcFallbackTest cached" 1234 cached
  , TestLabel "Exception" $ TestCase $ do
      env <- mkEnv
      rbad <- Control.Exception.try $ runHaxl env (dataFetch reqBad)
      assertBool "dcFallbackTest not found" $
        case rbad of
          Left (ErrorCall "no fetch defined") -> True
          _ -> False
  , TestLabel "Completions" $ TestCase $ do
      -- check applicative still runs as it would have without a fallback
      env <- mkEnv
      let
        fetchA = dataFetch reqEx
        fetchB = tellWrite 7 >> dataFetch req
      (rbad, writes) <- runHaxlWithWrites env $
        Haxl.Core.try $ (,) <$> fetchA <*> fetchB
      fetches <- countFetches env
      assertEqual "dispatched 2 fetches" 2 fetches
      assertBool "exception propogates" $
        case rbad of
          Left (NotFound _) -> True
          _ -> False
      assertEqual "write side effects happen" [7] writes
  ]
  where

    mkEnv = addLookup <$> initEnv (stateSet TestReqState stateEmpty) ()

    countFetches env = do
      (Stats stats) <- readIORef (statsRef env)
      let
        c = sum [ fetchBatchSize x
                | x@FetchStats{} <- stats
                ]
      return c

    addLookup :: Env () Int -> Env () Int
    addLookup e = e { dataCacheFetchFallback = Just (DataCacheLookup lookup)
                    , flags = (flags e) { report = profilingReportFlags }
                    }
    lookup
      :: forall req a . Typeable (req a)
      => req a
      -> IO (Maybe (ResultVal a Int))
    lookup r
      | typeOf r == typeRep (Proxy :: Proxy (TestReq Int)) =
        -- have to coerce on the way out as results are not Typeable
        -- so you better be sure you do it right!
        return $ unsafeCoerce . doReq <$> cast r
      | typeOf r == typeRep (Proxy :: Proxy (CacheableReq Int)) =
          return $ unsafeCoerce . doCache <$> cast r
      | otherwise = return Nothing

    doReq :: TestReq Int -> ResultVal Int Int
    doReq (Req 999) = ThrowHaxl (toException $ NotFound empty) NilWrites
    doReq (Req r) = Ok r NilWrites

    doCache :: CacheableReq Int -> ResultVal Int Int
    doCache (CacheableInt i) = Ok i NilWrites

    req :: TestReq Int
    req = Req 1

    reqEx :: TestReq Int
    reqEx = Req 999

    reqBad :: TestReq String
    reqBad = Req 2


-- tests :: Assertion
tests = TestList [ dcSoundnessTest
                 , dcStrictnessTest
                 , dcFallbackTest
                 ]
