module Network.Cloudant.Core where

import           Control.Monad.Reader
import           Data.Monoid                       ((<>))
import           Network.Cloudant.Databases        as Database
import           Network.Cloudant.Document         as Document
import           Network.Cloudant.Internal.Request (runRequest)
import           Network.Cloudant.Internal.Types

urlForAccount :: String -> String
urlForAccount account = "https://" <> account <> ".cloudant.com"

getHTTPEndpoint :: String -> String -> String
getHTTPEndpoint account resource = concat [ "https://", account, ".cloudant.com", resource ]

-- Local configuration for testing etc
localConfig :: Config
localConfig = Config {
    url = "http://192.168.59.103",
    apiKey = ApiKey "admin" "password"
}

testRun req user pass =
    runRequest cfg req
    where cfg = Config "https://owainlewis.cloudant.com" (ApiKey user pass)
