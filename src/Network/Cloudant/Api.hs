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

import           Control.Applicative
import           Control.Lens
import           Control.Monad              (mzero)
import           Data.Aeson
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map                   as M
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

slash :: String -> String
slash s = "/" <> s

transformJSON :: (FromJSON a) => IO (Either String LBS.ByteString) -> IO (Maybe a)
transformJSON response = do
  r <- response
  case r of
    Left e     -> return mzero
    Right json -> return . decode $ json

-- Security
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

data GenerateAPIKey = GenerateAPIKey { generateAPIKeyAccount :: String }

data GenerateAPIKeyResponse = GenerateAPIKeyResponse {
    password :: String
  , ok       :: Bool
  , key      :: String
} deriving ( Show, Eq )

instance FromJSON GenerateAPIKeyResponse where
    parseJSON (Object o) = GenerateAPIKeyResponse <$> o .: "password" <*> o .: "ok" <*> o .: "key"
    parseJSON _ = mzero

instance Cloudant GenerateAPIKey where
    getResource (GenerateAPIKey account) = getHTTPEndpoint account "/_api/v2/api_keys"

-- Generate a new API key for your account
generateAPIKey :: String -> Auth -> IO (Maybe GenerateAPIKeyResponse)
generateAPIKey account auth =
    transformJSON response :: IO (Maybe GenerateAPIKeyResponse)
    where response = post (getResource $ GenerateAPIKey account) auth Nothing

-- Databases

-- 1. Create database
data CreateDatabase = CreateDatabase {
    createDatabaseAccount  :: String
  , createDatabaseDatabase :: String
} deriving ( Show, Eq )

instance Cloudant CreateDatabase where
    getResource (CreateDatabase account database) =
        getHTTPEndpoint account (slash database)

createDatabase :: String -> Auth -> String -> IO (Either String LBS.ByteString)
createDatabase account auth database =
    put (getResource $ CreateDatabase account database) auth Nothing

-- 2. Read database
data ReadDatabase = ReadDatabase {
    readDatabaseAccount  :: String
  , readDatabaseDatabase :: String
} deriving ( Show, Eq )

instance Cloudant ReadDatabase where
    getResource (ReadDatabase account database) =
        getHTTPEndpoint account ("/" <> database)

readDatabase account auth database =
    get (getResource $ ReadDatabase account database) auth Nothing

-- 3. Get databases
data GetDatabases = GetDatabases { getDatabasesAccount :: String }
  deriving ( Show, Eq )

instance Cloudant GetDatabases where
    getResource (GetDatabases account) = getHTTPEndpoint account "/_all_dbs"

getDatabases :: String -> Auth -> IO (Maybe [String])
getDatabases account auth =
    transformJSON response :: IO (Maybe [String])
    where response = get (getResource $ GetDatabases account) auth Nothing

-- 4. Get documents
data GetDocuments = GetDocuments {
    getDocumentsAccount  :: String
  , getDocumentsDatabase :: String
} deriving ( Show, Eq )

instance Cloudant GetDocuments where
    getResource (GetDocuments account database) =
        getHTTPEndpoint account (slash database)

-- 5. Get changes

-- 6. Delete

data DeleteDatabase = DeleteDatabase {
    deleteDatabaseAccount  :: String
  , deleteDatabaseDatabase :: String
} deriving ( Show, Eq )

instance Cloudant DeleteDatabase where
    getResource (DeleteDatabase account database) =
        getHTTPEndpoint account (slash database)

deleteDatabase account auth database =
    delete (getResource $ DeleteDatabase account database) auth Nothing

-- *****************************************************************
-- Documents
-- *****************************************************************

data CreateDocument = CreateDocument {
    createDocumentAccount  :: String
  , createDocumentDatabase :: String
  , createDocumentDocument :: M.Map String String
} deriving ( Show, Eq )

instance Cloudant CreateDocument where
    getResource (CreateDocument account database _) =
        getHTTPEndpoint account (slash database)

createDocument account auth database document =
    post resource auth (Just $ documentAsJSON document)
    where resource = (getResource $ CreateDocument account database document)
          documentAsJSON = LBS.toStrict . encode
