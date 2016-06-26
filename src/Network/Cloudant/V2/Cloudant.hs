{-# LANGUAGE OverloadedStrings #-}
module Network.Cloudant.V2.Cloudant where

import           Network.HTTP.Dispatch.Core
import           Network.HTTP.Dispatch.Headers
import           Network.HTTP.Dispatch.Types
import qualified Data.ByteString             as S
import qualified Data.ByteString.Char8       as SC

data Config = Config {
    host :: S.ByteString
  , username :: S.ByteString
  , password :: S.ByteString
} deriving ( Show, Eq )

runCloudant (Config h u p) req = withHeader req (basicAuth u p)
