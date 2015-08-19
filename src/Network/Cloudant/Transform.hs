{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Cloudant.Transform where

import           Control.Applicative  ((<$>), (<*>))
import           Control.Monad        (mzero)
import           Data.Aeson
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import           GHC.Generics         (Generic)

#if __GLASGOW_HASKELL__ >= 706
#endif

-- | Strict encoding for Aeson
--
strictEncode :: ToJSON a => a -> BS.ByteString
strictEncode = LBS.toStrict . encode

-- | Given a response, try and convert it to a concrete type
--
transform :: (FromJSON a) => IO (Either String LBS.ByteString) -> IO (Either String a)
transform response = do
  r <- response
  case r of
    Left e    -> return $ Left e
    Right bs  -> case (decode bs) of
      Nothing     -> return $ Left "Cannot transform JSON"
      Just result -> return $ Right $ result

data IRPair = IRPair {
    _id  :: String
  , _rev :: String
} deriving ( Show, Eq, Generic )

instance FromJSON IRPair
instance ToJSON IRPair

data OKResponse = OKResponse {
    createdOk      :: Bool
  , createId       :: String
  , createRevision :: String
} deriving ( Show )

instance FromJSON OKResponse where
    parseJSON (Object o) =
        OKResponse <$> o .: "ok"
                   <*> o .: "id"
                   <*> o .: "rev"
    parseJSON _ = mzero

data DatabaseInfo = DatabaseInfo {
    dbName        :: String
  , documentCount :: Int
  , diskSize      :: Int
} deriving ( Show )

instance FromJSON DatabaseInfo where
    parseJSON (Object o) =
        DatabaseInfo <$> o .: "db_name"
                     <*> o .: "doc_count"
                     <*> o .: "disk_size"
    parseJSON _ = mzero
