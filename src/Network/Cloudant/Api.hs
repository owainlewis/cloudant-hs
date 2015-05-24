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

-- Requests
-- *****************************************************

data GetPermissions = GetPermissions {
    getPermissionsAccount  :: String
  , getPermissionsDatabase :: String
}

instance Cloudant GetPermissions where
    getResource x = getHTTPEndpoint userAccount resource
                    where userAccount = (getPermissionsAccount x)
                          resource    = "/_api/v2/db/" <> (getPermissionsDatabase x) <> "/_security"

-- Databases
-- ******************************************************

-- 1. Create database

data CreateDatabase = CreateDatabase {
    createDatabaseAccount  :: String
  , createDatabaseDatabase :: String }

instance Cloudant CreateDatabase where
    getResource x = getHTTPEndpoint account ("/" <> database)
        where account  = (createDatabaseAccount x)
              database = (createDatabaseDatabase x)

createDatabase :: String -> Auth -> String -> IO (Either String LBS.ByteString)
createDatabase account auth database =
    put (getResource $ CreateDatabase account database) auth Nothing

-- 2. Read database

-- 3. Get databases

data GetDatabases = GetDatabases { getDatabasesAccount :: String }

instance Cloudant GetDatabases where
    getResource x = getHTTPEndpoint userAccount "/_all_dbs"
        where userAccount = (getDatabasesAccount x)

getDatabases :: String -> Auth -> IO (Either String LBS.ByteString)
getDatabases account auth =
    get (getResource $ GetDatabases account) auth Nothing

-- 4. Get documents
-- 5. Get changes
-- 6. Delete
