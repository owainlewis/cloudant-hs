{-# LANGUAGE OverloadedStrings #-}
module Network.Cloudant.Api
  ( getWithAuth )
  where

import           Control.Lens
import           Data.Aeson
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as L
-- import qualified Data.Map                   as M
import           Data.Monoid                (mconcat, (<>))
import           Network.Wreq

-- Resource endpoints

class Cloudant a where
    getResource :: a -> String

urlForAccount :: String -> String
urlForAccount account = "https://" <> account <> ".cloudant.com"

getHTTPEndpoint :: String -> String -> String
getHTTPEndpoint account resource = mconcat [ "https://", account, ".cloudant.com", resource ]

buildAuth user pass = defaults & auth ?~ basicAuth (BS.pack user) (BS.pack pass)

getWithAuth :: String -> String -> String -> IO L.ByteString
getWithAuth url user pass = do
    let opts = defaults & auth ?~ basicAuth (BS.pack user) (BS.pack pass)
    r <- getWith opts url
    return $ (r ^. responseBody)

postWithAuth :: ToJSON a =>  String -> String -> String -> a -> IO L.ByteString
postWithAuth url user pass body = do
    let opts = buildAuth user pass
    r <- postWith opts url (toJSON body)
    return $ (r ^. responseBody)

runRequest :: Cloudant a => a -> String -> String -> IO L.ByteString
runRequest request user password =
  getWithAuth (getResource request) user password

-- Get permissions for a database
--
data GetPermissions = GetPermissions {
    account  :: String
  , database :: String
}
instance Cloudant GetPermissions where
    getResource x = getHTTPEndpoint userAccount resource
                    where userAccount = (account x)
                          resource    = "/_api/v2/db/" <> (database x) <> "/_security"

instance Show GetPermissions where show = getResource

-- Databases
-- 1. Create database
-- 2. Read database
-- 3. Get databases
-- 4. Get documents
-- 5. Get changes
-- 6. Delete

data GetDatabases = GetDatabases {

}

data CreateDatabase = CreateDatabase {
    createDatabaseAccount  :: String
  , createDatabaseDatabase :: String
}
