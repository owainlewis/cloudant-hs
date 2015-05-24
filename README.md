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

Create a document

```haskell

module Main where

import Network.Cloudant.Api

-- Assuming there is a database users that we have permission to write to

createDocument "username" ("user", "pass") "users" (M.fromList [("foo", "bar")])
-- Right "{\"ok\":true,\"id\":\"c6b903451addd368aa834fd51e984d0f\",\"rev\":\"1-4c6114c65e295552ab1019e2b046b10e\"}\n"

```
