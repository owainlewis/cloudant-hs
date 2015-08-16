module Network.Cloudant.Core where

import           Control.Monad.Reader
import           Network.Cloudant.Databases
import           Network.Cloudant.Internal.Request
import           Network.Cloudant.Internal.Types

-- Local configuration for testing etc
localConfig :: Config
localConfig = Config {
    url = "http://192.168.59.103:5984",
    apiKey = ApiKey "admin" "password"
}
