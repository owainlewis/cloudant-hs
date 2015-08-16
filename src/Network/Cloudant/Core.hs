module Network.Cloudant.Core where

import           Data.Aeson                        (ToJSON)
import qualified Network.Cloudant.Database         as Database
import qualified Network.Cloudant.Document         as Document
import           Network.Cloudant.Internal.Request (runRequest)
import           Network.Cloudant.Internal.Types   (Config (..))
import qualified Network.Cloudant.Transform        as TF

-- Database

createDatabase config database = response
    where response = runRequest config (Database.create database)

getDatabases :: Config -> IO (Either String [String])
getDatabases config = TF.transform response :: IO (Either String [String])
    where response = runRequest config (Database.all)

-- Document

createDocument :: ToJSON a => Config -> String -> a -> IO (Either String TF.OKResponse)
createDocument config database document =
    TF.transform response :: IO (Either String TF.OKResponse)
    where response = runRequest config (Document.create database document)
