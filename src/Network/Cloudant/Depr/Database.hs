module Network.Cloudant.Database where

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8        as LBS
import           Data.Monoid                       ((<>))
import           Network.Cloudant.Api
import           Network.Cloudant.Internal.Request

---------------------------------------------
-- | Databases
---------------------------------------------

-- | 1. Create a database
------------------------------------------------------------------

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

-- | 2. Read database
------------------------------------------------------------------

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

-- | 3. Get databases
------------------------------------------------------------------

data GetDatabases = GetDatabases { getDatabasesAccount :: String }
  deriving ( Show, Eq )

instance Cloudant GetDatabases where
    getResource (GetDatabases account) = getHTTPEndpoint account "/_all_dbs"

getDatabases :: String -> Auth -> IO (Maybe [String])
getDatabases account auth =
    transformJSON response :: IO (Maybe [String])
    where response = get (getResource $ GetDatabases account) auth Nothing

-- | 4. Get documents
------------------------------------------------------------------

data GetDocuments = GetDocuments {
    getDocumentsAccount  :: String
  , getDocumentsDatabase :: String
} deriving ( Show, Eq )

instance Cloudant GetDocuments where
    getResource (GetDocuments account database) =
        getHTTPEndpoint account (slash database)

getDocuments account auth database =
  response
  where response = get (getResource $ GetDocuments account database) auth Nothing

-- | 5. Get changes
------------------------------------------------------------------

-- | 6. Delete database
------------------------------------------------------------------

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

------------------------------------------------------------------
