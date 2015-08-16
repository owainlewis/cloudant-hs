{-# LANGUAGE OverloadedStrings #-}
module Network.Cloudant.Document where

import           Control.Applicative               ((<$>), (<*>))
import           Control.Monad                     (mzero)
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8        as LBS
import qualified Data.Map                          as M
import           Data.Monoid                       ((<>))
import           Network.Cloudant.Api
import           Network.Cloudant.Internal.Request
-- | Documents
---------------------------------------------

-- 1. Create a document
------------------------------------------------------------------

data CreateDocument = CreateDocument {
    createDocumentAccount  :: String
  , createDocumentDatabase :: String
  , createDocumentDocument :: M.Map String String
} deriving ( Show, Eq )

instance Cloudant CreateDocument where
    getResource (CreateDocument account database _) =
        getHTTPEndpoint account (slash database)

data CreateDocumentResponse = CreateDocumentResponse {
    createdOk      :: Bool
  , createId       :: String
  , createRevision :: String
} deriving ( Show, Eq )

instance FromJSON CreateDocumentResponse where
    parseJSON (Object o) =
        CreateDocumentResponse <$> o .: "ok"
                               <*> o .: "id"
                               <*> o .: "rev"
    parseJSON _ = mzero

-- Create a new Cloudant document
------------------------------------------------------------------

createDocument ::
      String
   -> Auth
   -> String
   -> M.Map String String
   -> IO (Maybe CreateDocumentResponse)
createDocument account auth database document =
    transformJSON response :: IO (Maybe CreateDocumentResponse)
    where resource = (getResource $ CreateDocument account database document)
          documentAsJSON = LBS.toStrict . encode
          response = post resource auth (Just $ documentAsJSON document)

data ReadDocument = ReadDocument {
    readDocumentAccount    :: String
  , readDocumentDatabase   :: String
  , readDocumentIdentifier :: String
} deriving ( Show, Eq )

instance Cloudant ReadDocument where
    getResource (ReadDocument account database id) =
        getHTTPEndpoint account $ (slash database) <> (slash id)

{-- Read a document
 -- The uncoding here is left to the user
 -- Here is an example for a Document with a string string map structure
 -- λ> let read = readDocument "account" ("user", "pass") "users"
 -- λ> read "e9b5771b00358fb98921b099e02706c9" :: IO (Maybe (M.Map String String))
 -- Just (fromList [("_id","e9b5771b00358fb98921b099e02706c9") ...
 --}
readDocument :: FromJSON a => String -> Auth -> String -> String -> IO (Maybe a)
readDocument account auth database id =
    transformJSON response
    where resource = getResource $ ReadDocument account database id
          response = get resource auth Nothing

data DeleteDocument = DeleteDocument {
    deleteDocumentAccount  :: String
  , deleteDocumentDatabase :: String
  , deleteDocumentRevision :: String
} deriving ( Show, Eq )

instance Cloudant DeleteDocument where
    getResource (DeleteDocument account database rev) =
        getHTTPEndpoint account $ (slash database) <> (slash rev)
