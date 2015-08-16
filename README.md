# Cloudant-hs

This package provides a Haskell interface to the IBM Cloudant database.
It should also have full compatibility with CouchDB

## Configuration

The configuration needed for Cloudant is fairly minimal. We need

1. The full host name of your database (e.g https://owainlewis.cloudant.com)
2. An API Key which is made up of a username and password

```haskell
-- Remote cloudant config
conf :: Config
conf = Config "https://owainlewis.cloudant.com" $ ApiKey "USER" "PASS"

-- Local configuration for testing against CouchDB
localConfig :: Config
localConfig = Config "http://192.168.59.103" $ ApiKey "admin" "password"
```

## Return types

All responses are in the form **IO (Either Error Response)**

## Example

```haskell
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

-- Create a new database
Cloudant.createDatabase localconfig "customers"

-- Add a document to the database
-- This will automatically serialize any document type with a ToJSON instance

-- The signature for createDocument is
-- createDocument :: ToJSON a => Config -> String -> a -> IO (Either String TF.OKResponse)

Cloudant.createDocument localConfig "customers" customer
    where customer = Customer "Jack" "Dorsey" "jack@twitter.com"

```

## Database Info

```haskell
λ> databaseInfo localConfig "customers"
Right (DatabaseInfo {dbName = "customers", documentCount = 3, diskSize = 16482})
```
