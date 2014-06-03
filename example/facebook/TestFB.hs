{-# LANGUAGE RebindableSyntax, OverloadedStrings #-}
module Main (main) where

import Control.Exception as E
import Data.Aeson
import Data.HashMap.Strict ((!))
import Data.Time.Calendar
import Data.Time.Clock
import FB
import FB.DataSource
import Haxl.Core
import Haxl.Prelude
import System.Environment
import System.Exit
import System.IO.Error
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import qualified Data.Vector as Vector

main = do
  (creds, access_token) <- getCredentials
  facebookState <- initGlobalState 10 creds access_token
  env <- initEnv (stateSet facebookState stateEmpty) ()
  r <- runHaxl env $ do
    likes <- getObject "me/likes"
    mapM getObject (likeIds likes)      -- these happen concurrently
  print r

likeIds :: Object -> [Id]
likeIds likes = do
  Array arr <- [likes ! "data"]
  Object obj <- Vector.toList arr
  String id <- [obj ! "id"]
  return (Id id)

-- Modifed from the test in the fb package:
-- https://github.com/meteficha/fb/blob/master/tests/Main.hs
-- Copyright (c)2012, Felipe Lessa

-- | Grab the Facebook credentials from the environment.
getCredentials :: IO (Credentials, UserAccessToken)
getCredentials = tryToGet `E.catch` showHelp
    where
      tryToGet = do
        [appName, appId, appSecret, accessToken] <-
           mapM getEnv ["APP_NAME", "APP_ID", "APP_SECRET", "ACCESS_TOKEN"]
        now <- getCurrentTime
        let creds = Credentials (T.pack appName)
                                (T.pack appId)
                                (T.pack appSecret)
            access_token = UserAccessToken
                             (Id "me")
                             (T.pack accessToken)
                             now
        return (creds, access_token)

      showHelp exc | not (isDoesNotExistError exc) = E.throw exc
      showHelp _ = do
        putStrLn $ unlines
          [ "In order to run the tests from the 'haxl-facebook' package, you"
          , "need developer access to a Facebook app.  Create an app by"
          , "going to http://developers.facebook.com, select \"Create a New"
          , " App\" from the \"Apps\" menu at the top.  Then create an"
          , "access token using the Graph API explorer:"
          , "   https://developers.facebook.com/tools/explorer"
          , "Select your app from the \"Application\" menu at the top, then hit"
          , "\"Get Access Token\".  The access token will last about 2 hours."
          , ""
          , "Please supply your app's name, id and secret in the environment"
          , "variables APP_NAME, APP_ID and APP_SECRET, respectively, and"
          , "the access token in ACCESS_TOKEN."
          , ""
          , "For example, before running the test you could run in the shell:"
          , ""
          , " $ export APP_NAME=\"test\""
          , " $ export APP_ID=\"000000000000000\""
          , " $ export APP_SECRET=\"xxxxxxxxxxxxxxxxxxxxxxxxxxxx\""
          , " $ export ACCESS_TOKEN=\"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\""
          , ""
          , "Of course, these values above aren't valid and you need to"
          , "replace them with your own."
          , ""
          , "(Exiting now with a failure code.)"]
        exitFailure
