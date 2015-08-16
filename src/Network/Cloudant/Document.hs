module Network.Cloudant.Document where

import           Data.Aeson
import           Data.Monoid                       ((<>))
import           Network.Cloudant.Internal.Request
import qualified Network.Cloudant.Transform        as T

-- Create a Cloudant document
create :: ToJSON s => String -> s -> RequestBuilder
create database document = RequestBuilder POST (withSlash database) (Just json) Nothing
    where json = T.strictEncode . toJSON $ document

get database id = RequestBuilder GET ((withSlash database) <> (withSlash id)) Nothing Nothing
