{-# LANGUAGE OverloadedStrings, StandaloneDeriving, RecordWildCards,
    GADTs, TypeFamilies, MultiParamTypeClasses, DeriveDataTypeable,
    FlexibleInstances #-}
-- QSem was deprecated in 7.6, but no more
{-# OPTIONS_GHC -fno-warn-deprecations #-}
module FB.DataSource
  ( FacebookReq(..)
  , initGlobalState
  , Credentials(..)
  , UserAccessToken
  , AccessToken(..)
  ) where

import Network.HTTP.Conduit
import Facebook as FB
import Control.Monad.Trans.Resource
import Data.Hashable
import Data.Typeable
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Conduit
import Data.Conduit.List hiding (mapM, mapM_)
import Data.Monoid
import Data.Aeson
import Control.Concurrent.Async
import Control.Concurrent.QSem
import Control.Exception

import Haxl.Core

data FacebookReq a where
   GetObject      :: Id -> FacebookReq Object
   GetUser        :: UserId -> FacebookReq User
   GetUserFriends :: UserId -> FacebookReq [Friend]
  deriving Typeable

deriving instance Eq (FacebookReq a)
deriving instance Show (FacebookReq a)

instance Show1 FacebookReq where show1 = show

instance Hashable (FacebookReq a) where
  hashWithSalt s (GetObject (Id id))      = hashWithSalt s (0::Int,id)
  hashWithSalt s (GetUser (Id id))        = hashWithSalt s (1::Int,id)
  hashWithSalt s (GetUserFriends (Id id)) = hashWithSalt s (2::Int,id)

instance StateKey FacebookReq where
  data State FacebookReq =
    FacebookState
       { credentials :: Credentials
       , userAccessToken :: UserAccessToken
       , manager :: Manager
       , numThreads :: Int
       }

instance DataSourceName FacebookReq where
  dataSourceName _ = "Facebook"

instance DataSource u FacebookReq where
  fetch = facebookFetch

initGlobalState
  :: Int
  -> Credentials
  -> UserAccessToken
  -> IO (State FacebookReq)

initGlobalState threads creds token = do
  manager <- newManager tlsManagerSettings
  return FacebookState
    { credentials = creds
    , manager = manager
    , userAccessToken = token
    , numThreads = threads
    }

facebookFetch
  :: State FacebookReq
  -> Flags
  -> u
  -> [BlockedFetch FacebookReq]
  -> PerformFetch

facebookFetch FacebookState{..} _flags _user bfs =
  AsyncFetch $ \inner -> do
    sem <- newQSem numThreads
    asyncs <- mapM (fetchAsync credentials manager userAccessToken sem) bfs
    inner
    mapM_ wait asyncs

fetchAsync
  :: Credentials -> Manager -> UserAccessToken -> QSem
  -> BlockedFetch FacebookReq
  -> IO (Async ())
fetchAsync creds manager tok sem (BlockedFetch req rvar) =
  async $ bracket_ (waitQSem sem) (signalQSem sem) $ do
    e <- Control.Exception.try $
           runResourceT $ runFacebookT creds manager $ fetchReq tok req
    case e of
      Left ex -> putFailure rvar (ex :: SomeException)
      Right a -> putSuccess rvar a

fetchReq
  :: UserAccessToken
  -> FacebookReq a
  -> FacebookT Auth (ResourceT IO) a

fetchReq tok (GetObject (Id id)) =
  getObject ("/" <> id) [] (Just tok)

fetchReq _tok (GetUser id) =
  getUser id [] Nothing

fetchReq tok (GetUserFriends id) = do
  f <- getUserFriends id [] tok
  source <- fetchAllNextPages f
  source $$ consume
