module FB
  ( getObject
  , getUser
  , getUserFriends
  , Id(..), Friend(..), User(..)
  ) where

import FB.DataSource
import Data.Aeson
import Facebook (Id(..), Friend(..), User(..))

import Haxl.Core

-- | Fetch an arbitrary object in the Facebook graph.
getObject :: Id -> GenHaxl u Object
getObject id = dataFetch (GetObject id)

-- | Fetch a Facebook user.
getUser :: Id -> GenHaxl u User
getUser id = dataFetch (GetUser id)

-- | Fetch the friends of a Facebook user that are registered with the
-- current app.
getUserFriends :: Id -> GenHaxl u [Friend]
getUserFriends id = dataFetch (GetUserFriends id)
