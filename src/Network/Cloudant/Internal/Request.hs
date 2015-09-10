{-# LANGUAGE OverloadedStrings #-}
module Network.Cloudant.Internal.Request where

import qualified Control.Exception               as E
import qualified Data.ByteString.Char8           as BS
import qualified Data.ByteString.Lazy.Char8      as LBS
import           Data.List                       (isPrefixOf)
import           Data.Maybe                      (fromJust, fromMaybe)
import           Data.Monoid                     ((<>))
import           Data.Text                       (Text)
import           Network.Cloudant.Internal.Types (ApiKey (..), Config (..))
import           Network.HTTP.Conduit

data HTTPMethod = GET | POST | PUT | DELETE deriving ( Show )

withSlash :: String -> String
withSlash s = "/" <> s

-- | Build a base request with authentication
build ::
    HTTPMethod ->
    String -> -- The resource endpoint e.g /all_dbs
    ApiKey ->
    Maybe BS.ByteString -> -- An optional request body
    IO Request
build reqMethod url apiKey body = do
    let remoteRequest = "https" `isPrefixOf` url
        reqBody  = fromMaybe (BS.pack "") body
        u = BS.pack $ username apiKey
        p = BS.pack $ password apiKey
        uri = applyBasicAuth u p $ fromJust $ parseUrl url
        request  = uri { method = (BS.pack . show $ reqMethod)
                       , secure = if remoteRequest then True else False
                       , requestHeaders =
                           (requestHeaders uri) <> [("Content-Type", "application/json")]
                       , requestBody = RequestBodyBS reqBody
                       , port = if remoteRequest then 443 else 5984
                       }
    return request

httpGet :: String -> ApiKey -> Maybe BS.ByteString -> IO Request
httpGet    = build GET

httpPost :: String -> ApiKey -> Maybe BS.ByteString -> IO Request
httpPost   = build POST

httpPut :: String -> ApiKey -> Maybe BS.ByteString -> IO Request
httpPut    = build PUT

httpDelete :: String -> ApiKey -> Maybe BS.ByteString -> IO Request
httpDelete = build DELETE

withParams :: Functor f => f Request -> [(BS.ByteString, Maybe BS.ByteString)] -> f Request
withParams req params = (setQueryString params) `fmap` req

-- | Run a HTTP request returning the response body
--
run :: IO Request -> IO LBS.ByteString
run request = do
    req <- request
    manager <- newManager tlsManagerSettings
    response <- httpLbs req manager
    return . responseBody $ response

runSafe :: IO Request -> IO (Either String LBS.ByteString)
runSafe request = do
    response <- E.try (run request) :: IO (Either E.SomeException LBS.ByteString)
    case response of
        Left  e -> return . Left $ (show e)
        Right r -> return . Right $ r

data RequestBuilder = RequestBuilder {
    reqMethod   :: HTTPMethod
  , reqResource :: String
  , reqBody     :: Maybe BS.ByteString
  , reqParams   :: Maybe [(BS.ByteString, Maybe BS.ByteString)]
} deriving ( Show )

asRequest :: Config -> RequestBuilder -> IO Request
asRequest conf RequestBuilder { reqMethod = m,  reqResource = r, reqBody = b,  reqParams = p } =
  let fullResource = (url conf) <> r
      key = apiKey conf
      underlyingRequest = build m fullResource key b in
  underlyingRequest

runRequest :: Config -> RequestBuilder -> IO (Either String LBS.ByteString)
runRequest conf = runSafe . (asRequest conf)
