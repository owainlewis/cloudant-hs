{-# LANGUAGE OverloadedStrings #-}
module Network.Cloudant.Api
  ( )
  where

import           Control.Lens
import           Data.Aeson
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Maybe                 (fromJust, fromMaybe)
import           Data.Monoid                (mconcat, (<>))
import           Network.HTTP.Conduit

-- Basic authentication tuple
type Auth = (String, String)

user :: Auth -> String
user auth = auth ^. _1

pass :: Auth -> String
pass auth = auth ^. _2

-- Build a request with basic authentication
--
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

-- Make a HTTP request
-- HTTP method, url, basic authentication and an optional request body
--
-- Example :
--     makeRequest "https://httpbin.org/get" ("jack", "secret") Nothing
--
makeRequest ::
    String -> -- HTTP Method e.g GET
    String ->
    Auth   ->
    Maybe BS.ByteString ->
    IO LBS.ByteString
makeRequest method url auth body =
    runRequest $ buildRequest method url auth body

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

instance Show GetPermissions where show = getResource

-- Databases
-- 1. Create database
data CreateDatabase = CreateDatabase {
    createDatabaseAccount  :: String
  , createDatabaseDatabase :: String
}
-- 2. Read database
-- 3. Get databases
data GetDatabases = GetDatabases { getDatabasesAccount :: String }

instance Cloudant GetDatabases where
    getResource x = getHTTPEndpoint userAccount "/_all_dbs"
        where userAccount = (getDatabasesAccount x)

getDatabases :: String -> Auth -> IO LBS.ByteString
getDatabases account auth =
    makeRequest "GET" (getResource $ GetDatabases account) auth Nothing
-- 4. Get documents
-- 5. Get changes
-- 6. Delete
