{-# LANGUAGE OverloadedStrings #-}
module Network.Cloudant.Request where

import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Maybe                 (fromJust, fromMaybe)
import           Network.HTTP.Conduit

doHTTPSRequest ::
  String ->  -- method i.e GET
  String ->  -- HTTPS endpoint
  String ->  -- Basic auth user
  String ->  -- Basic auth password
  Maybe BS.ByteString ->
  IO (LBS.ByteString)
doHTTPSRequest reqMethod url user pass body = do
  let reqBody  = fromMaybe (BS.pack "") body
      uri = applyBasicAuth (BS.pack user) (BS.pack pass) $ fromJust $ parseUrl url
      request  = uri { method = (BS.pack reqMethod)
                     , secure = True
                     , port = 443
                     }
  r <- withManager $ httpLbs request
  return (responseBody r)

-- testRequest = doHTTPSRequest "GET" "https://httpbin.org/get" Nothing
