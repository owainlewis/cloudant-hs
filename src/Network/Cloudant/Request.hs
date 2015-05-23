{-# LANGUAGE OverloadedStrings #-}
module Network.Cloudant.Request
  ( Auth
  , buildRequest
  , runRequest
  , makeRequest
  ) where

import           Control.Lens
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Maybe                 (fromJust, fromMaybe)
import           Network.HTTP.Conduit

-- Basic authentication tuple
type Auth = (String, String)

user :: Auth -> String
user auth = auth ^. _1

pass :: Auth -> String
pass auth = auth ^. _2

-- Build a request with basic authentication
--
-- Example:
--
--     buildRequest "GET" "https://httpbin.org/get" ("", "") Nothing
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
