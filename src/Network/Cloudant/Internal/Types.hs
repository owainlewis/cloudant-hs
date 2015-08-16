{-# LANGUAGE OverloadedStrings #-}
module Network.Cloudant.Internal.Types
  ( ApiKey(..)
  , Config(..)
  ) where

-- An API key used for authentication
-- This is made up of a username and password which get sent via basic auth
data ApiKey = ApiKey {
    username :: String
  , password :: String
} deriving ( Show )

-- Cloudant API configuration
--
data Config = Config {
    url    :: String
  , apiKey :: ApiKey
} deriving ( Show )

-- A Cloudant document contains an id, revision and attributes
--
data Document a = Document {
    id      :: String
  , rev     :: String
  , content :: a
} deriving ( Show, Eq )
