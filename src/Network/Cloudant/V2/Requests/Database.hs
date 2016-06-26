{-# LANGUAGE OverloadedStrings #-}
module Network.Cloudant.V2.Requests.Database where

import Network.HTTP.Dispatch.Core

getDatabases = get "https://owainlewis.cloudant.com/_all_dbs"
