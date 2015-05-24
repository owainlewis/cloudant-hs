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

import qualified Control.Exception          as E
import           Control.Lens
import           Data.Aeson
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Maybe                 (fromJust, fromMaybe)
import           Data.Monoid                (mconcat, (<>))
import           Network.HTTP.Conduit

type Auth = ( String, String )

user :: Auth -> String
user auth = auth ^. _1

pass :: Auth -> String
pass auth = auth ^. _2

-- Build a request with basic authentication
buildRequest ::
    String ->
    String ->
    Auth ->
    Maybe BS.ByteString ->
    IO Request
buildRequest reqMethod url auth body = do
    let reqBody  = fromMaybe (BS.pack "") body
        uri = applyBasicAuth (BS.pack $ user auth) (BS.pack $ pass auth) $ fromJust $ parseUrl url
        request  = uri { method = (BS.pack reqMethod)
                       , secure = True
                       , port = 443
                       }
    return request

-- Run a HTTP request returning the response body
--
runRequest :: IO Request -> IO LBS.ByteString
runRequest request = do
    req <- request
    response <- withManager $ httpLbs req
    return . responseBody $ response

catchAny :: IO a -> (E.SomeException -> IO a) -> IO a
catchAny = E.catch

safeRequest :: IO Request -> IO (Either String LBS.ByteString)
safeRequest request = do
    response <- E.try (runRequest request) :: IO (Either E.SomeException LBS.ByteString)
    case response of
        Left  e -> return . Left $ "Error. Could not run request"
        Right r -> return . Right $ r

makeRequest ::
    String -> -- HTTP Method e.g GET
    String ->
    Auth   ->
    Maybe BS.ByteString ->
    IO (Either String LBS.ByteString)
makeRequest method url auth body =
    safeRequest $ buildRequest method url auth body

-- Helper methods
get, post, put, delete ::
    String ->
    Auth ->
    Maybe BS.ByteString ->
    IO (Either String LBS.ByteString)
get    = makeRequest "GET"
post   = makeRequest "POST"
put    = makeRequest "PUT"
delete = makeRequest "DELETE"

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
