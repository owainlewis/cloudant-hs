# Cloudant-hs

Haskell client for Cloudant

# Examples

Generating an API key

```haskell

module Main where

import Network.Cloudant.Api

-- Generate a new API key

-- generateAPIKey "yourusername" ("username", "password")
-- Just (GenerateAPIKeyResponse {password = "cloudantPassword", ok = True, key = "cloudantKey"})

```
