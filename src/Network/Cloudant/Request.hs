{-# LANGUAGE OverloadedStrings #-}
module Network.Cloudant.Request
  ( Auth
  , user
  , pass
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
import           Network.HTTP.Conduit

type Auth = (String, String)

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
    IO (Either String LBS.ByteString)
makeRequest method url auth body =
    safeRequest $ buildRequest method url auth body
