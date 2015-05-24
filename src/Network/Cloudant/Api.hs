{-# LANGUAGE OverloadedStrings #-}
module Network.Cloudant.Api
  ( Auth
  , user
  , pass
  , urlForAccount
  , getHTTPEndpoint
  , Cloudant(..)
  , CreateDatabase(..)
  , createDatabase
  , GetDatabases(..)
  , getDatabases
  ) where

import           Control.Lens
import           Data.Aeson
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Maybe                 (fromJust, fromMaybe)
import           Data.Monoid                (mconcat, (<>))
import           Network.Cloudant.Request

class Cloudant a where
    getResource :: a -> String

urlForAccount :: String -> String
urlForAccount account = "https://" <> account <> ".cloudant.com"

getHTTPEndpoint :: String -> String -> String
getHTTPEndpoint account resource =
    mconcat [ "https://"
            , account
            , ".cloudant.com"
            , resource ]

-- Security
-- -----------------------------------------
data GetPermissions = GetPermissions {
    getPermissionsAccount  :: String
  , getPermissionsDatabase :: String
}

instance Cloudant GetPermissions where
    getResource (GetPermissions account database) = getHTTPEndpoint account resource
        where resource = "/_api/v2/db/" <> database <> "/_security"

getPermissions :: String -> Auth -> String -> IO (Either String LBS.ByteString)
getPermissions account auth database =
    get (getResource $ GetPermissions account database) auth Nothing

data GenerateAPIKey = GenerateAPIKey { generateAPIKeyAccount :: String }

instance Cloudant GenerateAPIKey where
    getResource (GenerateAPIKey account) = getHTTPEndpoint account "/_api/v2/api_keys"

-- Generate a new API key for your account
generateAPIKey :: String -> Auth -> IO (Either String LBS.ByteString)
generateAPIKey account auth =
    post (getResource $ GenerateAPIKey account) auth Nothing

-- Databases
--|-----------------------------------------

-- 1. Create database
--|-----------------------------------------
data CreateDatabase = CreateDatabase {
    createDatabaseAccount  :: String
  , createDatabaseDatabase :: String }

instance Cloudant CreateDatabase where
    getResource (CreateDatabase account database) =
        getHTTPEndpoint account ("/" <> database)

createDatabase :: String -> Auth -> String -> IO (Either String LBS.ByteString)
createDatabase account auth database =
    put (getResource $ CreateDatabase account database) auth Nothing

-- 2. Read database
--|-----------------------------------------
data ReadDatabase = ReadDatabase {
    readDatabaseAccount  :: String
  , readDatabaseDatabase :: String
}

instance Cloudant ReadDatabase where
    getResource x = getHTTPEndpoint (readDatabaseAccount x) ("/" <> readDatabaseDatabase x)

readDatabase account auth database =
    get (getResource $ ReadDatabase account database) auth Nothing

-- 3. Get databases
--|-----------------------------------------
data GetDatabases = GetDatabases { getDatabasesAccount :: String }

instance Cloudant GetDatabases where
    getResource (GetDatabases account) = getHTTPEndpoint account "/_all_dbs"

getDatabases :: String -> Auth -> IO (Either String LBS.ByteString)
getDatabases account auth =
    get (getResource $ GetDatabases account) auth Nothing

-- 4. Get documents
--|-----------------------------------------
data GetDocuments = GetDocuments {
    getDocumentsAccount  :: String
  , getDocumentsDatabase :: String
}

instance Cloudant GetDocuments where
    getResource (GetDocuments account database) = getHTTPEndpoint account database

-- 5. Get changes
--|-----------------------------------------

-- 6. Delete
--|-----------------------------------------

