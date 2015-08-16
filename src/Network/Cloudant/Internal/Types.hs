module Network.Cloudant.Internal.Types
  ( ApiKey(..)
  , Config(..)
  ) where

data ApiKey = ApiKey {
    username :: String
  , password :: String
} deriving ( Show )

data Config = Config {
    url    :: String
  , apiKey :: ApiKey
} deriving ( Show )
