module Network.Cloudant.Database where

import           Network.Cloudant.Internal.Request

-- | List all databases
--
all :: RequestBuilder
all = RequestBuilder GET "/_all_dbs" Nothing Nothing

{--
  To create a database, make a PUT request to https://$USERNAME.cloudant.com/$DATABASE.

  The database name must start with a lowercase letter and contain only the following characters

  Lowercase characters (a-z)
  Digits (0-9)
  Any of the characters _, $, (, ), +, -, and /

  There are two configuration parameters that control the sharding topology of a database.

  The defaults are specified in the server configuration and may be overridden at database
  creation time on dedicated database clusters. n specifies the number of replicas of each
  document, while q fixes the number of partitions of the database.

  On multi-tenant clusters, the defaults can not be overwritten.
-}
create :: String -> RequestBuilder
create database =
    RequestBuilder PUT (withSlash database) Nothing Nothing

-- | Delete a database
delete :: String -> RequestBuilder
delete database =
    RequestBuilder DELETE (withSlash database) Nothing Nothing

{--
  Making a GET request against https://$USERNAME.cloudant.com/$DATABASE
  returns details about the database, such as how many documents it contains.
--}
info :: String -> RequestBuilder
info database =
    RequestBuilder GET (withSlash database) Nothing Nothing
