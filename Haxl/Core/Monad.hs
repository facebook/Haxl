-- Copyright (c) 2014, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file. An additional grant of patent rights can
-- be found in the PATENTS file.

{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The implementation of the 'Haxl' monad.
module Haxl.Core.Monad (
    -- * The monad
    GenHaxl (..), runHaxl,
    env,

    -- * Exceptions
    throw, catch, catchIf, try, tryToHaxlException,

    -- * Data fetching and caching
    dataFetch, uncachedRequest,
    cacheRequest, cacheResult, cachedComputation,
    dumpCacheAsHaskell,

    -- * Unsafe operations
    unsafeLiftIO, unsafeToHaxlException,
  ) where

import Haxl.Core.Types
import Haxl.Core.Fetch
import Haxl.Core.Env
import Haxl.Core.Exception
import Haxl.Core.RequestStore
import Haxl.Core.Util
import Haxl.Core.DataCache

import qualified Data.Text as Text
import Control.Exception (Exception(..), SomeException)
import qualified Control.Exception
import Control.Applicative hiding (Const)
import GHC.Exts (IsString(..))
#if __GLASGOW_HASKELL__ < 706
import Prelude hiding (catch)
#endif
import Data.IORef
import Data.Monoid
import Text.Printf
import Text.PrettyPrint hiding ((<>))
import Control.Arrow (left)

-- -----------------------------------------------------------------------------
-- | The Haxl monad, which does several things:
--
--  * It is a reader monad for 'Env' and 'IORef' 'RequestStore', The
--    latter is the current batch of unsubmitted data fetch requests.
--
--  * It is a concurrency, or resumption, monad. A computation may run
--    partially and return 'Blocked', in which case the framework should
--    perform the outstanding requests in the 'RequestStore', and then
--    resume the computation.
--
--  * The Applicative combinator '<*>' explores /both/ branches in the
--    event that the left branch is 'Blocked', so that we can collect
--    multiple requests and submit them as a batch.
--
--  * It contains IO, so that we can perform real data fetching.
--
newtype GenHaxl u a = GenHaxl
  { unHaxl :: Env u -> IORef (RequestStore u) -> IO (Result u a) }

-- | The result of a computation is either 'Done' with a value, 'Throw'
-- with an exception, or 'Blocked' on the result of a data fetch with
-- a continuation.
data Result u a
  = Done a
  | Throw SomeException
  | Blocked (GenHaxl u a)

instance (Show a) => Show (Result u a) where
  show (Done a) = printf "Done(%s)" $ show a
  show (Throw e) = printf "Throw(%s)" $ show e
  show Blocked{} = "Blocked"

instance Monad (GenHaxl u) where
  return a = GenHaxl $ \_env _ref -> return (Done a)
  GenHaxl m >>= k = GenHaxl $ \env ref -> do
    e <- m env ref
    case e of
      Done a       -> unHaxl (k a) env ref
      Throw e      -> return (Throw e)
      Blocked cont -> return (Blocked (cont >>= k))

instance Functor (GenHaxl u) where
  fmap f m = pure f <*> m

instance Applicative (GenHaxl u) where
  pure = return
  GenHaxl f <*> GenHaxl a = GenHaxl $ \env ref -> do
    r <- f env ref
    case r of
      Throw e -> return (Throw e)
      Done f' -> do
        ra <- a env ref
        case ra of
          Done a'    -> return (Done (f' a'))
          Throw e    -> return (Throw e)
          Blocked a' -> return (Blocked (f' <$> a'))
      Blocked f' -> do
        ra <- a env ref  -- left is blocked, explore the right
        case ra of
          Done a'    -> return (Blocked (f' <*> return a'))
          Throw e    -> return (Blocked (f' <*> throw e))
          Blocked a' -> return (Blocked (f' <*> a'))

-- | Runs a 'Haxl' computation in an 'Env'.
runHaxl :: Env u -> GenHaxl u a -> IO a
runHaxl env (GenHaxl haxl) = do
  ref <- newIORef noRequests
  e <- haxl env ref
  case e of
    Done a       -> return a
    Throw e      -> Control.Exception.throw e
    Blocked cont -> do
      bs <- readIORef ref
      performFetches env bs
      runHaxl env cont

-- | Extracts data from the 'Env'.
env :: (Env u -> a) -> GenHaxl u a
env f = GenHaxl $ \env _ref -> return (Done (f env))

-- -----------------------------------------------------------------------------
-- Exceptions

-- | Throw an exception in the Haxl monad
throw :: (Exception e) => e -> GenHaxl u a
throw e = GenHaxl $ \_env _ref -> raise e

raise :: (Exception e) => e -> IO (Result u a)
raise = return . Throw . toException

-- | Catch an exception in the Haxl monad
catch :: Exception e => GenHaxl u a -> (e -> GenHaxl u a) -> GenHaxl u a
catch (GenHaxl m) h = GenHaxl $ \env ref -> do
   r <- m env ref
   case r of
     Done a    -> return (Done a)
     Throw e | Just e' <- fromException e -> unHaxl (h e') env ref
             | otherwise -> return (Throw e)
     Blocked k -> return (Blocked (catch k h))

-- | Catch exceptions that satisfy a predicate
catchIf
  :: Exception e => (e -> Bool) -> GenHaxl u a -> (e -> GenHaxl u a)
  -> GenHaxl u a
catchIf cond haxl handler =
  catch haxl $ \e -> if cond e then handler e else throw e

-- | Returns @'Left' e@ if the computation throws an exception @e@, or
-- @'Right' a@ if it returns a result @a@.
try :: Exception e => GenHaxl u a -> GenHaxl u (Either e a)
try haxl = (Right <$> haxl) `catch` (return . Left)

-- -----------------------------------------------------------------------------
-- Unsafe operations

-- | Under ordinary circumstances this is unnecessary; users of the Haxl
-- monad should generally /not/ perform arbitrary IO.
unsafeLiftIO :: IO a -> GenHaxl u a
unsafeLiftIO m = GenHaxl $ \_env _ref -> Done <$> m

-- | Convert exceptions in the underlying IO monad to exceptions in
-- the Haxl monad.  This is morally unsafe, because you could then
-- catch those exceptions in Haxl and observe the underlying execution
-- order.  Not to be exposed to user code.
unsafeToHaxlException :: GenHaxl u a -> GenHaxl u a
unsafeToHaxlException (GenHaxl m) = GenHaxl $ \env ref -> do
  r <- m env ref `Control.Exception.catch` \e -> return (Throw e)
  case r of
    Blocked c -> return (Blocked (unsafeToHaxlException c))
    other -> return other

-- | Like 'try', but lifts all exceptions into the 'HaxlException'
-- hierarchy.  Uses 'unsafeToHaxlException' internally.  Typically
-- this is used at the top level of a Haxl computation, to ensure that
-- all exceptions are caught.
tryToHaxlException :: GenHaxl u a -> GenHaxl u (Either HaxlException a)
tryToHaxlException h = left asHaxlException <$> try (unsafeToHaxlException h)

-- -----------------------------------------------------------------------------
-- Data fetching and caching

-- | Performs actual fetching of data for a 'Request' from a 'DataSource'.
dataFetch :: (DataSource u r, Request r a) => r a -> GenHaxl u a
dataFetch req = GenHaxl $ \env ref -> do
  -- First, check the cache
  res <- cached env req
  case res of
    -- Not seen before: add the request to the RequestStore, so it
    -- will be fetched in the next round.
    Uncached rvar -> do
      modifyIORef' ref $ \bs -> addRequest (BlockedFetch req rvar) bs
      return $ Blocked (continueFetch req rvar)

    -- Seen before but not fetched yet.  We're blocked, but we don't have
    -- to add the request to the RequestStore.
    CachedNotFetched rvar -> return
      $ Blocked (continueFetch req rvar)

    -- Cached: either a result, or an exception
    Cached (Left ex) -> return (Throw ex)
    Cached (Right a) -> return (Done a)

-- | A data request that is not cached.  This is not what you want for
-- normal read requests, because then multiple identical requests may
-- return different results, and this invalidates some of the
-- properties that we expect Haxl computations to respect: that data
-- fetches can be aribtrarily reordered, and identical requests can be
-- commoned up, for example.
--
-- 'uncachedRequest' is useful for performing writes, provided those
-- are done in a safe way - that is, not mixed with reads that might
-- conflict in the same Haxl computation.
--
uncachedRequest :: (DataSource u r, Request r a) => r a -> GenHaxl u a
uncachedRequest req = GenHaxl $ \_env ref -> do
  rvar <- newEmptyResult
  modifyIORef' ref $ \bs -> addRequest (BlockedFetch req rvar) bs
  return $ Blocked (continueFetch req rvar)

continueFetch
  :: (DataSource u r, Request r a, Show a)
  => r a -> ResultVar a -> GenHaxl u a
continueFetch req rvar = GenHaxl $ \_env _ref -> do
  m <- tryReadResult rvar
  case m of
    Nothing -> raise . DataSourceError $
      textShow req <> " did not set contents of result var"
    Just (Left e) -> return (Throw e)
    Just (Right a) -> return (Done a)

-- | Transparently provides caching. Useful for datasources that can
-- return immediately, but also caches values.
cacheResult :: (Request r a)  => r a -> IO a -> GenHaxl u a
cacheResult req val = GenHaxl $ \env _ref -> do
  cachedResult <- cached env req
  case cachedResult of
    Uncached rvar -> do
      result <- Control.Exception.try val
      putResult rvar result
      done result
    Cached result -> done result
    CachedNotFetched _ -> corruptCache
  where
    corruptCache = raise . DataSourceError $ Text.concat
      [ textShow req
      , " has a corrupted cache value: these requests are meant to"
      , " return immediately without an intermediate value. Either"
      , " the cache was updated incorrectly, or you're calling"
      , " cacheResult on a query that involves a blocking fetch."
      ]

-- | Inserts a request/result pair into the cache. Throws an exception
-- if the request has already been issued, either via 'dataFetch' or
-- 'cacheRequest'.
--
-- This can be used to pre-populate the cache when running tests, to
-- avoid going to the actual data source and ensure that results are
-- deterministic.
--
cacheRequest
  :: (Request req a) => req a -> Either SomeException a -> GenHaxl u ()
cacheRequest request result = GenHaxl $ \env _ref -> do
  res <- cached env request
  case res of
    Uncached rvar -> do
      -- request was not in the cache: insert the result and continue
      putResult rvar result
      return $ Done ()

    -- It is an error if the request is already in the cache.  We can't test
    -- whether the cached result is the same without adding an Eq constraint,
    -- and we don't necessarily have Eq for all results.
    _other -> raise $
      DataSourceError "cacheRequest: request is already in the cache"

instance IsString a => IsString (GenHaxl u a) where
  fromString s = return (fromString s)

-- | 'cachedComputation' memoizes a Haxl computation.  The key is a
-- request.
--
-- /Note:/ These cached computations will /not/ be included in the output
-- of 'dumpCacheAsHaskell'.
--
cachedComputation
   :: forall req u a. (Request req a)
   => req a -> GenHaxl u a -> GenHaxl u a
cachedComputation req haxl = GenHaxl $ \env ref -> do
  res <- memoized env req
  case res of
    -- Uncached: we must compute the result and store it in the ResultVar.
    Uncached rvar -> do
      let
          with_result :: Either SomeException a -> GenHaxl u a
          with_result r = GenHaxl $ \_ _ -> do putResult rvar r; done r

      unHaxl (try haxl >>= with_result) env ref

    -- CachedNotFetched: this request is already being computed, we just
    -- have to block until the result is available.  Note that we might
    -- have to block repeatedly, because the Haxl computation might block
    -- multiple times before it has a result.
    CachedNotFetched rvar -> return $ Blocked (continueCached rvar)
    Cached r -> done r

continueCached :: ResultVar a -> GenHaxl u a
continueCached rvar = GenHaxl $ \_env _ref -> do
  m <- tryReadResult rvar
  case m of
    -- Unlike dataFetch, Nothing is not an error here: the computation
    -- is being worked on elsewhere and probably got blocked in a
    -- datafetch, we just have to keep waiting for the result.
    Nothing -> return $ Blocked (continueCached rvar)
    Just r -> done r

-- | Lifts an 'Either' into either 'Throw' or 'Done'.
done :: Either SomeException a -> IO (Result u a)
done = return . either Throw Done

-- | Dump the contents of the cache as Haskell code that, when
-- compiled and run, will recreate the same cache contents.  For
-- example, the generated code looks something like this:
--
-- > loadCache :: GenHaxl u ()
-- > loadCache = do
-- >   cacheRequest (ListWombats 3) (Right ([1,2,3]))
-- >   cacheRequest (CountAardvarks "abcabc") (Right (2))
--
dumpCacheAsHaskell :: GenHaxl u String
dumpCacheAsHaskell = do
  ref <- env cacheRef  -- NB. cacheRef, not memoRef.  We ignore memoized
                       -- results when dumping the cache.
  entries <- unsafeLiftIO $ readIORef ref >>= showCache
  let
    mk_cr (req, res) =
      text "cacheRequest" <+> parens (text req) <+> parens (result res)
    result (Left e) = text "except" <+> parens (text (show e))
    result (Right s) = text "Right" <+> parens (text s)

  return $ show $
    text "loadCache :: GenHaxl u ()" $$
    text "loadCache = do" $$
      nest 2 (vcat (map mk_cr (concatMap snd entries))) $$
    text "" -- final newline
