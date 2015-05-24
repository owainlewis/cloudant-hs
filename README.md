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

# Tutorial

## Creating documents

In order to create documents you must first have a database configured with the correct permissions for a given user.
Visit the cloudant console if you aren't sure how to do this.

This library uses a simple Haskell Map to represent documents.

```haskell

module Main where

import Network.Cloudant.Api

-- Assuming there is a database users that we have permission to write to
createDocument "username" ("user", "pass") "users" (M.fromList [("foo", "bar")])
-- Right "{\"ok\":true,\"id\":\"c6b903451addd368aa834fd51e984d0f\",\"rev\":\"1-4c6114c65e295552ab1019e2b046b10e\"}\n"

```

One handy tip is to partially apply the createDocument function to save typing.

Here is an example

```haskell
λ> let create = createDocument "myaccount" ("user", "pass") "users"
λ> let document = M.fromList [("foo", "bar"), ("baz", "foo")]
λ> create document

Just (CreateDocumentResponse {
  createdOk = True,
  createId = "97845b1aaa6d16dd85033670a7ed7601",
  createRevision = "1-d65d8e5cd6ff7ceeec7fcec5e79a7669"
})

```
