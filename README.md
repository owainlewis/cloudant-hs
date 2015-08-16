# Cloudant-hs

This package provides a Haskell interface to the IBM Cloudant database.

## Overview

All requests and responses follow a uniform approach.

You will need your account name and either a (username, password) or (username,  api-key) tuple for authentication

```
https://<USERNAME>.cloudant.com
```

## Authentication

```haskell
module Main where

import Network.Cloudant.Core

-- Local configuration for testing against CouchDB
localConfig :: Config
localConfig = Config {
    url = "http://192.168.59.103",
    apiKey = ApiKey "admin" "password"
}

-- Create a database

runRequest localConfig $ createDatabase "customers"

```

# Tutorial
