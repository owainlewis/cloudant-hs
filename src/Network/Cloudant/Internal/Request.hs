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
import           Data.Maybe                 (fromJust, fromMaybe)
import           Data.Text                  (Text)
import           Network.HTTP.Conduit

-- Transformers

import           Control.Exception
import           Control.Monad.Except

---------------------------------------

data Auth = Auth { username :: String, password :: String }
    deriving ( Show, Ord, Eq )

data RequestType = GET | POST | PUT | DELETE deriving ( Show )

type QueryParams = [(BS.ByteString, Maybe BS.ByteString)]

-- | Build a request with basic authentication
buildRequest ::
    String ->
    String ->
    Auth ->
    Maybe BS.ByteString ->
    IO Request
buildRequest reqMethod url auth body = do
    let reqBody  = fromMaybe (BS.pack "") body
        user = BS.pack $ username auth
        pass = BS.pack $ password auth
        uri = applyBasicAuth user pass $ fromJust $ parseUrl url
        request  = uri { method = (BS.pack reqMethod)
                       , secure = True
                       , requestHeaders = [("Content-Type", "application/json")]
                       , requestBody = RequestBodyBS reqBody
                       , port = 443
                       }
    return request

-- | Helper methods
get, post, put, delete ::
    String ->
    Auth ->
    Maybe BS.ByteString ->
    IO (Either String LBS.ByteString)
get    = makeRequest "GET"
post   = makeRequest "POST"
put    = makeRequest "PUT"
delete = makeRequest "DELETE"

-- | Run a HTTP request returning the response body
--
runRequest :: IO Request -> IO LBS.ByteString
runRequest request = do
    req <- request
    response <- withManager $ httpLbs req
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

-- | Make a HTTP request
-- HTTP method, url, basic authentication and an optional request body
--
-- Example :
--     makeRequest "https://httpbin.org/get" (Auth "ibm", "secret") Nothing
--
makeRequest ::
    String              -> -- HTTP Method e.g GET
    String              -> -- The URL
    Auth                -> -- Basic authentication creds
    Maybe BS.ByteString -> -- Optional request body
    IO (Either String LBS.ByteString)
makeRequest method url auth body =
    safeRequest $ buildRequest method url auth body

-- | Make a HTTP request with additional query params
--
requestWithParams ::
    String                                 -> -- HTTP Method
    String                                 -> -- URL
    Auth                                   -> -- Basic authentication
    Maybe BS.ByteString                    -> -- Optional request body
    [(BS.ByteString, Maybe BS.ByteString)] -> -- A list of query params
    IO (Either String LBS.ByteString)
requestWithParams method url auth body params =
    safeRequest $ (setQueryString params) `fmap` initialRequest
        where initialRequest = buildRequest method url auth body
