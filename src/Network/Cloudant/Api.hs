{-# LANGUAGE OverloadedStrings #-}
module Network.Cloudant.Api where

import           Control.Lens
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Monoid                (mconcat, (<>))
import           Network.Wreq

-- Resource endpoints

urlForAccount :: String -> String
urlForAccount account = "https://" <> account <> ".cloudant.com"

getHTTPEndpoint ::
  String ->
  String ->
  String
getHTTPEndpoint account resource =
    mconcat [ "https://", account, ".cloudant.com", resource ]

class Cloudant a where
    resourceEndpoint :: a -> String -> String

-- Simple GET requests
getWithAuth :: IO L.ByteString
getWithAuth = do
    let opts = defaults & auth ?~ basicAuth "myuser" "mypass"
    r <- getWith opts "http://httpbin.org/get"
    return $ (r ^. responseBody)

-- Types

data GetDatabases = GetDatabases {
} deriving ( Show )

instance Cloudant GetDatabases where
    resourceEndpoint _ account = getHTTPEndpoint account "/all_dbs"
