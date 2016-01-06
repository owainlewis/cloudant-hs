{-# LANGUAGE OverloadedStrings #-}
module Network.Cloudant.Util where

import           Data.Aeson                        (ToJSON, encode, toJSON)
import qualified Data.ByteString                   as BS
import qualified Data.ByteString.Lazy              as LBS
import           Data.Monoid                       (mconcat, (<>))
import           Network.Cloudant.Internal.Request (withSlash)
import           Network.Cloudant.Internal.Types

-- | Generate the full IBM Cloudant URL for an account
urlForAccount :: String -> String
urlForAccount account = "https://" <> account <> ".cloudant.com"

-- | Local configuration for testing against CouchDB
localConfig :: Config
localConfig = Config "http://192.168.59.103" $ ApiKey "admin" "password"

-- | Helper function that concats URL parts with a slash
--   e.g foldPaths ["foo", "bar"] => "/foo/bar"
foldPaths :: [String] -> String
foldPaths = mconcat . map withSlash

-- | Strict encoding for Aeson
strictEncode :: ToJSON a => a -> BS.ByteString
strictEncode = LBS.toStrict . encode

asJSONStrict :: ToJSON a => a -> BS.ByteString
asJSONStrict = strictEncode . toJSON
