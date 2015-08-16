module Network.Cloudant.Transform where

import           Data.Aeson
import qualified Data.ByteString.Lazy as LBS

-- | Given a repsonse try and convert it to a given type
--
transformJSON :: (FromJSON a) => IO (Either String LBS.ByteString) -> IO (Either String a)
transformJSON response = do
  r <- response
  case r of
    Left e    -> return $ Left e
    Right bs  -> case decode $ bs of
      Nothing     -> return $ Left "Cannot transform JSON"
      Just result -> return $ Right $ result
