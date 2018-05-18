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
import Control.Monad
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

instance ShowP FacebookReq where showp = show

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
       , semaphore :: QSem
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
  sem <- newQSem threads
  return FacebookState
    { credentials = creds
    , manager = manager
    , userAccessToken = token
    , semaphore = sem
    }

facebookFetch
  :: State FacebookReq
  -> Flags
  -> u
  -> PerformFetch FacebookReq

facebookFetch FacebookState{..} _flags _user =
  BackgroundFetch $
    mapM_ (fetchAsync credentials manager userAccessToken semaphore)

fetchAsync
  :: Credentials -> Manager -> UserAccessToken -> QSem
  -> BlockedFetch FacebookReq
  -> IO ()
fetchAsync creds manager tok sem (BlockedFetch req rvar) =
  void $ async $ bracket_ (waitQSem sem) (signalQSem sem) $ do
    e <- Control.Exception.try $
           runResourceT $ runFacebookT creds manager $ fetchFBReq tok req
    case e of
      Left ex -> putFailure rvar (ex :: SomeException)
      Right a -> putSuccess rvar a

fetchFBReq
  :: UserAccessToken
  -> FacebookReq a
  -> FacebookT Auth (ResourceT IO) a

fetchFBReq tok (GetObject (Id id)) =
  getObject ("/" <> id) [] (Just tok)

fetchFBReq _tok (GetUser id) =
  getUser id [] Nothing

fetchFBReq tok (GetUserFriends id) = do
  f <- getUserFriends id [] tok
  source <- fetchAllNextPages f
  source $$ consume
