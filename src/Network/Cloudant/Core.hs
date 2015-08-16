{-# LANGUAGE TypeOperators #-}
module Network.Cloudant.Core where

import           Data.Aeson                        (ToJSON)
import qualified Data.ByteString.Lazy              as LBS
import qualified Network.Cloudant.Database         as Database
import qualified Network.Cloudant.Document         as Document
import           Network.Cloudant.Internal.Request (runRequest)
import           Network.Cloudant.Internal.Types   (Config (..))
import qualified Network.Cloudant.Transform        as TF
import           Network.Cloudant.Util             (localConfig)

-- Database
createDatabase :: Config
  -> String
  -> IO (Either String LBS.ByteString)
createDatabase config database = response
    where response = runRequest config (Database.create database)

getDatabases :: Config -> IO (Either String [String])
getDatabases config = TF.transform response :: IO (Either String [String])
    where response = runRequest config (Database.all)

databaseInfo config database =
    TF.transform response :: IO (Either String TF.DatabaseInfo)
    where response = runRequest config (Database.info database)

-- Document

createDocument :: ToJSON document =>
  Config ->
  String ->
  document ->
  IO (Either String TF.OKResponse)
createDocument config database document =
    TF.transform response :: IO (Either String TF.OKResponse)
    where response = runRequest config (Document.create database document)
