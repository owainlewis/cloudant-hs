module Network.Cloudant.View where

import qualified Data.ByteString                   as BS
import           Network.Cloudant.Internal.Request
import           Network.Cloudant.Util             (foldPaths)

-- Views are used to obtain data stored within a database.
-- Views are written using Javascript functions.

create :: String -> String -> BS.ByteString -> RequestBuilder
create database designDoc view = RequestBuilder PUT path (Just view) Nothing
    where path = foldPaths [ database
                           , "_design"
                           , designDoc
                           ]
