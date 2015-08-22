module Network.Cloudant.Core where

import           Data.Aeson                        (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy              as LBS
import qualified Network.Cloudant.Database         as Database
import qualified Network.Cloudant.Document         as Document
import           Network.Cloudant.Internal.Request (runRequest)
import           Network.Cloudant.Internal.Types   (Config (..), Document (..))
import qualified Network.Cloudant.Transform        as TF
import           Network.Cloudant.Util             (localConfig)

-- Database
-----------------------------------------------------------------

createDatabase ::
  Config -> -- ^ configuration
  String -> -- ^ database name
  IO (Either String LBS.ByteString)
createDatabase config database = runRequest config (Database.create database)

deleteDatabase
  :: Config -> String -> IO (Either String LBS.ByteString)
deleteDatabase config database = runRequest config (Database.delete database)

getDatabases ::
  Config -> -- ^ configuration
  IO (Either String [String])
getDatabases config =
    TF.transform response :: IO (Either String [String])
      where response = runRequest config (Database.all)

databaseInfo ::
  Config -> -- ^ configuration
  String -> -- ^ database name
  IO (Either String TF.DatabaseInfo)
databaseInfo config database =
    TF.transform response :: IO (Either String TF.DatabaseInfo)
      where response = runRequest config (Database.info database)

-- Document
-----------------------------------------------------------------

-- Create a document in Cloudant
--
createDocument :: ToJSON document =>
  Config ->   -- ^ configuration
  String ->   -- ^ database name
  document -> -- ^ a type to insert with a ToJSON instance
  IO (Either String TF.OKResponse)
createDocument config database document =
    TF.transform response :: IO (Either String TF.OKResponse)
      where response = runRequest config (Document.create database document)

-- Get document ID and revision
--
getDocumentIR ::
   Config ->
   String ->
   String ->
   IO (Either String TF.IRPair)
getDocumentIR config database id =
    TF.transform response :: IO (Either String TF.IRPair)
      where response = runRequest config (Document.get database id)

getDocumentRaw
  :: Config
     -> Document.Database
     -> Document.ID
     -> IO (Either String LBS.ByteString)
getDocumentRaw config database id =
    runRequest config (Document.get database id)

-- Update

updateDocument config database id rev = ()

-- | Deltee
-- Delete a document from Cloudant
deleteDocument :: Config -> String -> String -> a -> IO (Either String LBS.ByteString)
deleteDocument config database id rev =
  runRequest config (Document.delete database id rev)

-----------------------------------------------------------------
