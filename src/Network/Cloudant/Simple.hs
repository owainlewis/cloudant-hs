{-# LANGUAGE OverloadedStrings #-}

-- | This package contains a stripped down and simplified API to reduce
--   the complexity of the initial design
--
module Network.Cloudant.Simple where

import           Data.Aeson                        (FromJSON, decode)
import           Data.ByteString.Lazy              as LBS
import           Data.Monoid                       ((<>))
import           Data.Text                         (Text)
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

-- | Given a repsonse try and convert it to a given type
--
transformJSON :: (FromJSON a) => IO (Either String LBS.ByteString) -> IO (Either String a)
transformJSON response = do
  r <- response
  case r of
    Left e    -> return $ Left e
    Right bs  -> case decode $ bs of
      Nothing     -> return $ Left "Cannot transform JSON"
      Just result -> return $ Right $ result

getDatabases :: Config -> IO (Either String ByteString)
getDatabases config =
  let endpoint = (configUrl config) <> "/_all_dbs"
      response = get endpoint (authFromConfig config) Nothing in
  response
