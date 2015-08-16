{-# LANGUAGE OverloadedStrings #-}
module Network.Cloudant.Security where

import           Control.Applicative               ((<$>), (<*>))
import           Control.Monad                     (mzero)
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8        as LBS
import           Data.Monoid                       ((<>))
import           Network.Cloudant.Api
import           Network.Cloudant.Internal.Request

-- | Get permissions
----------------------------------------------------

data GetPermissions = GetPermissions {
    getPermissionsAccount  :: String
  , getPermissionsDatabase :: String
} deriving ( Show, Eq )

instance Cloudant GetPermissions where
    getResource (GetPermissions account database) = getHTTPEndpoint account resource
        where resource = "/_api/v2/db/" <> database <> "/_security"

getPermissions :: String -> Auth -> String -> IO (Either String LBS.ByteString)
getPermissions account auth database =
    get (getResource $ GetPermissions account database) auth Nothing

-- | Generate API key
----------------------------------------------------

data GenerateAPIKey = GenerateAPIKey { generateAPIKeyAccount :: String }

data GenerateAPIKeyResponse = GenerateAPIKeyResponse {
    password :: String
  , ok       :: Bool
  , key      :: String
} deriving ( Show, Eq )

instance FromJSON GenerateAPIKeyResponse where
    parseJSON (Object o) =
        GenerateAPIKeyResponse <$> o .: "password"
                               <*> o .: "ok"
                               <*> o .: "key"
    parseJSON _ = mzero

instance Cloudant GenerateAPIKey where
    getResource (GenerateAPIKey account) = getHTTPEndpoint account "/_api/v2/api_keys"

-- Generate a new API key for your account
generateAPIKey :: String -> Auth -> IO (Maybe GenerateAPIKeyResponse)
generateAPIKey account auth =
    transformJSON response :: IO (Maybe GenerateAPIKeyResponse)
    where response = post (getResource $ GenerateAPIKey account) auth Nothing
