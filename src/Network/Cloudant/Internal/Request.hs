{-# LANGUAGE OverloadedStrings #-}
module Network.Cloudant.Internal.Request
  ( Auth(..)
  , makeRequest
  , get
  , post
  , put
  , delete
  ) where

import qualified Control.Exception          as E
import           Control.Lens
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.List                  (isPrefixOf)
import           Data.Maybe                 (fromJust, fromMaybe)
import           Data.Text                  (Text)
import           Network.HTTP.Conduit

-- Transformers

import           Control.Exception

---------------------------------------

data Auth = Auth { username :: String, password :: String }
    deriving ( Show, Ord, Eq )

data RequestType = GET | POST | PUT | DELETE deriving ( Show )

type QueryParams = [(BS.ByteString, Maybe BS.ByteString)]

-- | Build a request with basic authentication
buildRequest ::
    RequestType ->
    String ->
    Auth ->
    Maybe BS.ByteString ->
    IO Request
buildRequest reqMethod url auth body = do
    let remoteRequest = "https" `isPrefixOf` url
        reqBody  = fromMaybe (BS.pack "") body
        user = BS.pack $ username auth
        pass = BS.pack $ password auth
        uri = applyBasicAuth user pass $ fromJust $ parseUrl url
        request  = uri { method = (BS.pack . show $ reqMethod)
                       , secure = if remoteRequest then True else False
                       , requestHeaders = [("Content-Type", "application/json")]
                       , requestBody = RequestBodyBS reqBody
                       , port = if remoteRequest then 443 else 5984
                       }
    return request

-- | Make a HTTP request
-- HTTP method, url, basic authentication and an optional request body
--
-- Example :
--
--     λ> makeRequest "GET" "http://192.168.59.103" (Auth "admin" "password") Nothing
--
--
makeRequest ::
    RequestType         -> -- HTTP Method e.g GET
    String              -> -- The URL
    Auth                -> -- Basic authentication creds
    Maybe BS.ByteString -> -- Optional request body
    IO (Either String LBS.ByteString)
makeRequest method url auth body =
    safeRequest $ buildRequest method url auth body

-- | Helper methods
get, post, put, delete ::
    String -> -- Request URL
    Auth   -> -- Authentication credentials
    Maybe BS.ByteString -> -- A Request body
    IO (Either String LBS.ByteString)
get    = makeRequest GET
post   = makeRequest POST
put    = makeRequest PUT
delete = makeRequest DELETE

-- | Run a HTTP request returning the response body
--
runRequest :: IO Request -> IO LBS.ByteString
runRequest request = do
    req <- request
    manager <- newManager tlsManagerSettings
    response <- httpLbs req manager
    return . responseBody $ response

-- | Catch all exceptions
catchAny :: IO a -> (E.SomeException -> IO a) -> IO a
catchAny = E.catch

safeRequest :: IO Request -> IO (Either String LBS.ByteString)
safeRequest request = do
    response <- E.try (runRequest request) :: IO (Either E.SomeException LBS.ByteString)
    case response of
        Left  e -> return . Left $ (show e)
        Right r -> return . Right $ r

-- | Make a HTTP request with additional query params
--
requestWithParams ::
    RequestType                            -> -- HTTP Method
    String                                 -> -- URL
    Auth                                   -> -- Basic authentication
    Maybe BS.ByteString                    -> -- Optional request body
    [(BS.ByteString, Maybe BS.ByteString)] -> -- A list of query params
    IO (Either String LBS.ByteString)
requestWithParams method url auth body params =
    safeRequest $ (setQueryString params) `fmap` initialRequest
        where initialRequest = buildRequest method url auth body
