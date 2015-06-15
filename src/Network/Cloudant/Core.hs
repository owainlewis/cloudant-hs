{-# LANGUAGE FlexibleContexts #-}
module Network.Cloudant.Core
  ( ) where

import           Control.Monad.Reader
import qualified Data.ByteString.Lazy              as L
import           Network.Cloudant.Api
import           Network.Cloudant.Internal.Request

data Config = Config {
    cloudantUsername :: String
  , cloudantAuth     :: Auth
} deriving ( Show )

type AccountName = String
type Endpoint    = String

class C a where endpointFor :: AccountName -> a -> String

runRequest :: C a => a -> Reader Config (IO (Either String L.ByteString))
runRequest req = do
  config <- ask
  let auth = cloudantAuth config
  let endpoint = endpointFor (cloudantUsername config) req
  return $ get endpoint auth Nothing

runCloudant :: C a => a -> Config -> IO (Either String L.ByteString)
runCloudant req = runReader $ runRequest req
