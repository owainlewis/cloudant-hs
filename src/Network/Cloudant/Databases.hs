module Network.Cloudant.Databases
  ( getDatabases
  , createDatabase
  )
  where

import           Network.Cloudant.Internal.Request

getDatabases :: RequestBuilder
getDatabases = RequestBuilder GET "/_all_dbs" Nothing Nothing

createDatabase :: String -> RequestBuilder
createDatabase name =
  RequestBuilder PUT (withSlash name) Nothing Nothing
