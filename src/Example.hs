{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
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

-- | EXAMPLES
------------------------------------------------

-- | Create a database
example1 = Cloudant.createDatabase localConfig "customers"

-- | Delete a database
example2 = Cloudant.deleteDatabase localConfig "customers"

-- Insert documents
--
example3 :: IO ()
example3 = (flip mapM_) customers (Cloudant.createDocument localConfig "customers")
    where customers = [ Customer "Jack" "Dorsey" "jack@twitter.com"
                      , Customer "Owain" "Lewis" "owain@owainlewis.com"
                      ]

exampleView = unlines [ "function (doc) {"
                      , "    if (doc.email == \"owain@owainlewis.com\") {"
                      , "        emit(doc._id, doc);"
                      , "    }"
                      , "}"
                      ]
