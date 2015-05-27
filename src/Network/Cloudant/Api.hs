{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
--
-- Haskell wrapper for IBM Cloudant API
--
-- Cloudant's RESTful API makes every document in your Cloudant database accessible as JSON via a URL;
-- this is one of the features that makes Cloudant so powerful for web applications.
--
-- (c) Owain Lewis (2015)
--
-----------------------------------------------------------------------------

module Network.Cloudant.Api
  ( Cloudant(..)
  , urlForAccount
  , getHTTPEndpoint
  , slash
  , transformJSON
  ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad                     (mzero)
import           Data.Aeson
import qualified Data.ByteString.Char8             as BS
import qualified Data.ByteString.Lazy.Char8        as LBS
import qualified Data.Map                          as M
import           Data.Maybe                        (fromJust, fromMaybe)
import           Data.Monoid                       ((<>))
import           Network.Cloudant.Internal.Request

class Cloudant a where
    -- | Extract the full resource URL e.g https://user.cloudant.com/database
    getResource :: a -> String

urlForAccount :: String -> String
urlForAccount account = "https://" <> account <> ".cloudant.com"

getHTTPEndpoint :: String -> String -> String
getHTTPEndpoint account resource = concat [ "https://", account, ".cloudant.com", resource ]

-- Utility function to add a slash prefix to a given request path
--
slash :: String -> String
slash s = "/" <> s

-- Strict encoding for Aeson
strictEncode :: ToJSON a => a -> BS.ByteString
strictEncode = LBS.toStrict . encode

transformJSON :: (FromJSON a) => IO (Either String LBS.ByteString) -> IO (Maybe a)
transformJSON response = do
  r <- response
  case r of
    Left e     -> return mzero
    Right json -> return . decode $ json
