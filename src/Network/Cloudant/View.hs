module Network.Cloudant.View 
  ( create )
  where

import qualified Data.ByteString                   as BS
import           Network.Cloudant.Internal.Request
import           Network.Cloudant.Util             (foldPaths)

create :: String -> String -> BS.ByteString -> RequestBuilder
create database designDoc view = RequestBuilder PUT path (Just view) Nothing
    where path = foldPaths [ database, "_design", designDoc ]
