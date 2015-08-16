module Network.Cloudant.Database where

import           Network.Cloudant.Internal.Request

-- | List all databases
--
all :: RequestBuilder
all = RequestBuilder GET "/_all_dbs" Nothing Nothing

-- | Create a new database
--
create :: String -> RequestBuilder
create name =
  RequestBuilder PUT (withSlash name) Nothing Nothing
