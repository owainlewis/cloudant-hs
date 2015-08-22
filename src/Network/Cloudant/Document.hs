module Network.Cloudant.Document where

import           Data.Aeson
import           Data.Monoid                       (mconcat, (<>))
import           Network.Cloudant.Internal.Request
import qualified Network.Cloudant.Transform        as T

type Database = String
type ID       = String

foldPaths :: [String] -> String
foldPaths = mconcat . map withSlash

-- Create a Cloudant document
create :: ToJSON s => Database -> s -> RequestBuilder
create database document = RequestBuilder POST (withSlash database) (Just json) Nothing
    where json = asJSONStrict document
          asJSONStrict = T.strictEncode . toJSON

get :: Database -> ID -> RequestBuilder
get database id =
    RequestBuilder GET path Nothing Nothing
      where path = foldPaths [database, id]

-- Update

-- To update (or create) a document,
-- make a PUT request with the updated JSON content and the latest _rev
-- value (not needed for creating new documents) to
-- https://$USERNAME.cloudant.com/$DATABASE/$DOCUMENT_ID.

-- Delete
-- DELETE /$DATABASE/$DOCUMENT_ID?rev=$REV
delete :: String -> String -> a -> RequestBuilder
delete database id rev =
    RequestBuilder DELETE path Nothing Nothing
      where path = foldPaths [database, id]
