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

import Network.Cloudant.Api

auth :: Auth
auth = ("username", "password)
```

All request functions typically require your username and authentication credentials i.e

```haskell

repsonse :: IO (Maybe GenerateAPIKeyResponse)
response = generateAPIKey "<USERNAME>" auth

```

# Examples

Generating an API key

```haskell

module Main where

import Network.Cloudant.Api

-- Generate a new API key

-- generateAPIKey "yourusername" ("username", "password")
-- Just (GenerateAPIKeyResponse {password = "cloudantPassword", ok = True, key = "cloudantKey"})

```
