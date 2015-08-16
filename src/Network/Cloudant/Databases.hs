module Network.Cloudant.Databases
  ( getDatabases
  )
  where

import           Network.Cloudant.Internal.Request

getDatabases :: RequestBuilder
getDatabases = RequestBuilder GET "/_all_dbs" Nothing Nothing
