{-# LANGUAGE OverloadedStrings #-}
module Network.Cloudant.Api
  ( getWithAuth ) 
  where

import           Control.Lens
import qualified Data.ByteString.Char8      as BS
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
getWithAuth :: String -> String -> String -> IO L.ByteString
getWithAuth url user pass = do
    let opts = defaults & auth ?~ basicAuth (BS.pack user) (BS.pack pass)
    r <- getWith opts url
    return $ (r ^. responseBody)

-- Types

-- List all databases for an account
data GetDatabases = GetDatabases deriving ( Show )

instance Cloudant GetDatabases where
    resourceEndpoint _ account = getHTTPEndpoint account "/_all_dbs"
