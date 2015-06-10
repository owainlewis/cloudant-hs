{-# LANGUAGE FlexibleContexts #-}
module Network.Cloudant.Core where

import           Control.Monad.Reader
import qualified Data.ByteString.Lazy              as L
import           Network.Cloudant.Internal.Request

data Config = Config {
    cloudantUsername :: String
  , cloudantAuth     :: Auth
} deriving ( Show )

simpleGet :: MonadReader Config m => String -> m (IO (Either String L.ByteString))
simpleGet endpoint = do
  config <- ask
  let auth = (cloudantAuth config)
  return $ get endpoint auth Nothing
