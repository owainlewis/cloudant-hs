{-# LANGUAGE OverloadedStrings #-}
module Network.Cloudant.Api where

import           Control.Lens
import qualified Data.ByteString.Lazy.Char8 as L
import           Network.Wreq

-- Simple GET request

simpleGet :: IO L.ByteString
simpleGet = do
    r <- get "http://httpbin.org/get"
    return $ r ^. responseBody

getStatusCode :: IO Int
getStatusCode = do
    r <- get "http://httpbin.org/get"
    return $ r ^. responseStatus ^. statusCode

-- Request with basic Authentication

request :: IO L.ByteString
request = do
    let opts = defaults & auth ?~ basicAuth "myuser" "mypass"
    r <- getWith opts "http://httpbin.org/get"
    return $ (r ^. responseBody)
