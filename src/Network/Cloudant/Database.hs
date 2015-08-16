module Network.Cloudant.Database where

import           Network.Cloudant.Internal.Request

getAll :: RequestBuilder
getAll = RequestBuilder GET "/_all_dbs" Nothing Nothing

create :: String -> RequestBuilder
create name =
  RequestBuilder PUT (withSlash name) Nothing Nothing
