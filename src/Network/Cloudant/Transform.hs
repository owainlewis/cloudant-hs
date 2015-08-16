{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Cloudant.Transform where

import           Control.Applicative  ((<$>), (<*>))
import           Control.Monad        (mzero)
import           Data.Aeson
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import           GHC.Generics         (Generic)

-- | Strict encoding for Aeson
--
strictEncode :: ToJSON a => a -> BS.ByteString
strictEncode = LBS.toStrict . encode

-- | Given a repsonse try and convert it to a given type
--
transform :: (FromJSON a) => IO (Either String LBS.ByteString) -> IO (Either String a)
transform response = do
  r <- response
  case r of
    Left e    -> return $ Left e
    Right bs  -> case (decode bs) of
      Nothing     -> return $ Left "Cannot transform JSON"
      Just result -> return $ Right $ result

-- Response types
--
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
