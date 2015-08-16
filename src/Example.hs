{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Example where

import           Data.Aeson
import qualified Data.ByteString.Lazy  as LBS
import           GHC.Generics          (Generic)
import           Network.Cloudant.Core as Cloudant

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