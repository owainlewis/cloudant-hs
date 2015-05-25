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
  , GetDocuments(..)
  ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad              (mzero)
import           Data.Aeson
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map                   as M
import           Data.Maybe                 (fromJust, fromMaybe)
import           Data.Monoid                ((<>))
import           Network.Cloudant.Request

-- All request kinds must adhere to this method of generating
-- the resource url
--
class Cloudant a where
    -- | Extract the full resource URL e.g https://user.cloudant.com/database
    getResource :: a -> String

urlForAccount :: String -> String
urlForAccount account = "https://" <> account <> ".cloudant.com"

getHTTPEndpoint :: String -> String -> String
getHTTPEndpoint account resource = concat [ "https://", account, ".cloudant.com", resource ]

-- Utility function to add a slash prefix to a given request path
--
slash :: String -> String
slash s = "/" <> s

-- Strict encoding for Aeson
strictEncode :: ToJSON a => a -> BS.ByteString
strictEncode = LBS.toStrict . encode

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

-- | Databases
---------------------------------------------

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

readDatabase :: String -> Auth -> String -> IO (Either String LBS.ByteString)
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

deleteDatabase :: String -> Auth -> String -> IO (Either String LBS.ByteString)
deleteDatabase account auth database =
    delete (getResource $ DeleteDatabase account database) auth Nothing

-- | Documents
---------------------------------------------

data CreateDocument = CreateDocument {
    createDocumentAccount  :: String
  , createDocumentDatabase :: String
  , createDocumentDocument :: M.Map String String
} deriving ( Show, Eq )

instance Cloudant CreateDocument where
    getResource (CreateDocument account database _) =
        getHTTPEndpoint account (slash database)

data CreateDocumentResponse = CreateDocumentResponse {
    createdOk      :: Bool
  , createId       :: String
  , createRevision :: String
} deriving ( Show, Eq )

instance FromJSON CreateDocumentResponse where
    parseJSON (Object o) =
        CreateDocumentResponse <$> o .: "ok"
                               <*> o .: "id"
                               <*> o .: "rev"
    parseJSON _ = mzero

-- Create a new Cloudant document
--
-- Use a simpe Map for the document body
--
createDocument ::
      String
   -> Auth
   -> String
   -> M.Map String String
   -> IO (Maybe CreateDocumentResponse)
createDocument account auth database document =
    transformJSON response :: IO (Maybe CreateDocumentResponse)
    where resource = (getResource $ CreateDocument account database document)
          documentAsJSON = LBS.toStrict . encode
          response = post resource auth (Just $ documentAsJSON document)

data ReadDocument = ReadDocument {
    readDocumentAccount    :: String
  , readDocumentDatabase   :: String
  , readDocumentIdentifier :: String
} deriving ( Show, Eq )

instance Cloudant ReadDocument where
    getResource (ReadDocument account database id) =
        getHTTPEndpoint account $ (slash database) <> (slash id)

-- Read a document
-- The uncoding here is left to the user
--
-- Here is an example for a Document with a string string map structure
--
-- λ> let read = readDocument "account" ("user", "pass") "users"
-- λ> read "e9b5771b00358fb98921b099e02706c9" :: IO (Maybe (M.Map String String))
-- Just (fromList [("_id","e9b5771b00358fb98921b099e02706c9"),("_rev","1-d65d8e5cd6ff7ceeec7fcec5e79a7669"),("baz","foo"),("foo","bar")])
--
readDocument :: FromJSON a => String -> Auth -> String -> String -> IO (Maybe a)
readDocument account auth database id =
    transformJSON response
    where resource = getResource $ ReadDocument account database id
          response = get resource auth Nothing

data DeleteDocument = DeleteDocument {
    deleteDocumentAccount  :: String
  , deleteDocumentDatabase :: String
  , deleteDocumentRevision :: String
} deriving ( Show, Eq )

instance Cloudant DeleteDocument where
    getResource (DeleteDocument account database rev) =
        getHTTPEndpoint account $ (slash database) <> (slash rev)
