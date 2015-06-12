{-# LANGUAGE RebindableSyntax, OverloadedStrings #-}
module BatchTests (tests) where

import TestTypes
import TestUtils
import MockTAO

import Control.Applicative
import Test.HUnit

import Haxl.Core

import Prelude()
import Haxl.Prelude

-- -----------------------------------------------------------------------------

--
-- Test batching over multiple arguments in liftA2
--
batching1 = expectRounds 1 12 batching1_

batching1_ = do
  a <- id1
  b <- id2
  length <$> liftA2 (++) (friendsOf a) (friendsOf b)

--
-- Test batching in mapM (which is really traverse)
--
batching2 = expectRounds 1 12 batching2_

batching2_ = do
  a <- id1
  b <- id2
  fs <- mapM friendsOf [a,b]
  return (sum (map length fs))

--
-- Test batching when we have a monadic bind in each branch
--
batching3 = expectRounds 1 12 batching3_

batching3_ = do
  let a = id1 >>= friendsOf
      b = id2 >>= friendsOf
  length <$> a .++ b

--
-- Test batching over both arguments of (+)
--
batching4 = expectRounds 1 12 batching4_

batching4_ = do
  let a = length <$> (id1 >>= friendsOf)
      b = length <$> (id2 >>= friendsOf)
  a + b

--
-- Test batching over both arguments of (+)
--
batching5 = expectRounds 1 2 batching5_

batching5_ :: Haxl Int
batching5_ = if a .> b then 1 else 2
 where
  a = length <$> (id1 >>= friendsOf)
  b = length <$> (id2 >>= friendsOf)

--
-- Test batching when we perform all batching tests together with sequence
--
batching6 = expectRounds 1 [12,12,12,12,2] batching6_

batching6_ = sequence [batching1_,batching2_,batching3_,batching4_,batching5_]

--
-- Ensure if/then/else and bool operators break batching
--
batching7 = expectRounds 2 12 batching7_

batching7_ :: Haxl Int
batching7_ = if a .> 0 then a+b else 0
 where
  a = length <$> (id1 >>= friendsOf)
  b = length <$> (id2 >>= friendsOf)

-- We expect 3 rounds here due to boolean operators
batching8 = expectRounds 3 12 batching8_

batching8_ :: Haxl Int
batching8_ = if (c .== 0) .|| (a .> 0 .&& b .> 0) then a+b else 0
 where
  a = length <$> (id1 >>= friendsOf)
  b = length <$> (id2 >>= friendsOf)
  c = length <$> (id3 >>= friendsOf)

-- (>>) should batch, so we expect one round
batching9 = expectRounds 1 6 batching9_

batching9_ :: Haxl Int
batching9_ = (id1 >>= friendsOf) >> (length <$> (id2 >>= friendsOf))

--
-- Test data caching, numFetches
--

-- simple (one cache hit)
caching1 = expectFetches 3 caching1_
caching1_ = nf id1 + nf id2 + nf id3 + nf id3
 where
  nf id = length <$> (id >>= friendsOf)

-- simple, in rounds (no cache hits)
caching2 = expectFetches 3 caching2_
caching2_ = if nf id1 .> 0 then nf id2 + nf id3 else 0
 where
  nf id = length <$> (id >>= friendsOf)

-- rounds (one cache hit)
caching3 = expectFetches 3 caching3_
caching3_ = if nf id1 .> 0 then nf id1 + nf id2 + nf id3 else 0
 where
  nf id = length <$> (id >>= friendsOf)

--
-- Basic sanity check on data-cache re-use
--
cacheReuse = do
  env <- makeTestEnv
  expectRoundsWithEnv 2 12 batching7_ env

  -- make a new env
  tao <- MockTAO.initGlobalState
  let st = stateSet tao stateEmpty
  env2 <- initEnvWithData st testinput (caches env)

  -- ensure no more data fetching rounds needed
  expectRoundsWithEnv 0 12 batching7_ env2

exceptionTest1 = expectRounds 1 []
  $ withDefault [] $ friendsOf 101

exceptionTest2 = expectRounds 1 [7..12] $ liftA2 (++)
  (withDefault [] (friendsOf 101))
  (withDefault [] (friendsOf 2))

deterministicExceptions = do
  env <- makeTestEnv
  let haxl =
        sequence [ do _ <- friendsOf =<< id1; throw (NotFound "xxx")
                 , throw (NotFound "yyy")
                 ]
  -- the first time, friendsOf should block, but we should still get the
  -- "xxx" exception.
  r <- runHaxl env $ try haxl
  assertBool "exceptionTest3" $
    case r of
     Left (NotFound "xxx") -> True
     _ -> False
  -- the second time, friendsOf will be cached, and we should get the "xxx"
  -- exception as before.
  r <- runHaxl env $ try haxl
  assertBool "exceptionTest3" $
    case r of
     Left (NotFound "xxx") -> True
     _ -> False

tests = TestList
  [ TestLabel "batching1" $ TestCase batching1
  , TestLabel "batching2" $ TestCase batching2
  , TestLabel "batching3" $ TestCase batching3
  , TestLabel "batching4" $ TestCase batching4
  , TestLabel "batching5" $ TestCase batching5
  , TestLabel "batching6" $ TestCase batching6
  , TestLabel "batching7" $ TestCase batching7
  , TestLabel "batching8" $ TestCase batching8
  , TestLabel "batching9" $ TestCase batching9
  , TestLabel "caching1" $ TestCase caching1
  , TestLabel "caching2" $ TestCase caching2
  , TestLabel "caching3" $ TestCase caching3
  , TestLabel "CacheReuse" $ TestCase cacheReuse
  , TestLabel "exceptionTest1" $ TestCase exceptionTest1
  , TestLabel "exceptionTest2" $ TestCase exceptionTest2
  , TestLabel "deterministicExceptions" $ TestCase deterministicExceptions
  ]
