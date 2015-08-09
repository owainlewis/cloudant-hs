{-# LANGUAGE OverloadedStrings #-}

-- | This package contains a stripped down and simplified API to reduce
--   the complexity of the initial design
--
module Network.Cloudant.Simple where

import           Data.ByteString.Lazy              as LBS
import           Data.Monoid                       ((<>))
import           Data.Text                         (Text)
import           Network.Cloudant.Api              (transformJSON)
import           Network.Cloudant.Internal.Request

data Config = Config {
    configUrl      :: String -- TODO not strictly hostname i.e http://192.168.59.103
  , configUsername :: String
  , configPassword :: String
}

-- Local configuration for testing etc
localConfig :: Config
localConfig = Config {
    configUrl = "http://192.168.59.103:5984",
    configUsername = "admin",
    configPassword = "password"
}

authFromConfig :: Config -> Auth
authFromConfig config = Auth (configUsername config) (configPassword config)

-- TODO change the types to reflect errors etc
--
getDatabases :: Config -> IO (Maybe [String])
getDatabases config =
  let endpoint = (configUrl config) <> "/_all_dbs"
      response = get endpoint (authFromConfig config) Nothing
  in transformJSON response :: IO (Maybe [String])
