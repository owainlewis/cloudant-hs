# Cloudant-hs

This package provides a Haskell interface to the IBM Cloudant database.
It should also have full compatibility with CouchDB

## Configuration

The configuration needed for Cloudant is fairly minimal. We need

1. The full host name of your database (e.g https://owainlewis.cloudant.com)
2. An API Key which is made up of a username and password

```haskell
-- Local configuration for testing against CouchDB
localConfig :: Config
localConfig = Config {
    url = "http://192.168.59.103",
    apiKey = ApiKey "admin" "password"
}
```

## Example

```
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Example where

import           Data.Aeson
import qualified Data.ByteString.Lazy  as LBS
import           GHC.Generics          (Generic)
import           Network.Cloudant.Core as Cloudant
import           Network.Cloudant.Util (localConfig)

data Customer = Customer {
    firstName :: String
  , lastName  :: String
  , email     :: String
} deriving ( Show, Generic )

instance ToJSON Customer
instance FromJSON Customer

customer1 :: Customer
customer1 = Customer "Jack" "Dorsey" "jack@twitter.com"

asJSON :: ToJSON s => s -> LBS.ByteString
asJSON customer = encode . toJSON $ customer

-- Cloudant.createDatabase localconfig "customers"

-- Cloudant.createDocument localConfig "customers" (Customer "Jack" "Dorsey" "jack@twitter.com")

```
