{-# LANGUAGE OverloadedStrings #-}
module Network.Cloudant.Internal.Types 
    ( ApiKey(..)
    , Config(..)
    , Document(..)
    ) where

-- | An API key used for authentication
data ApiKey = ApiKey {
    username :: String
  , password :: String
} deriving ( Show )

-- | Cloudant API configuration
data Config = Config {
    url    :: String
  , apiKey :: ApiKey
} deriving ( Show )

-- | A Cloudant document contains an id, revision and attributes
data Document a = Document {
    id      :: String
  , rev     :: String
  , content :: a
} deriving ( Show, Eq )
