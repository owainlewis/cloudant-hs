module Network.Cloudant.Databases
  ( getDatabases
  , createDatabase
  )
  where

import           Data.Monoid                       ((<>))
import           Network.Cloudant.Internal.Request

getDatabases :: RequestBuilder
getDatabases = RequestBuilder GET "/_all_dbs" Nothing Nothing

createDatabase name =
  let endpoint = "/" <> name in
  RequestBuilder POST endpoint Nothing Nothing
