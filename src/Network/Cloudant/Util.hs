module Network.Cloudant.Util where

import           Data.Monoid                     ((<>))
import           Network.Cloudant.Internal.Types

-- Generate the full IBM Cloudant URL for an account
urlForAccount :: String -> String
urlForAccount account = "https://" <> account <> ".cloudant.com"

-- Local configuration for testing against CouchDB
--
localConfig :: Config
localConfig = Config "http://192.168.59.103" $ ApiKey "admin" "password"
