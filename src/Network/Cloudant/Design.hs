module Network.Cloudant.Design where

import           Network.Cloudant.Internal.Request
import           Network.Cloudant.Util             (foldPaths)

-- Instead of storing data in a document, you might also have special documents that store other content,
-- such as functions. The special documents are called “design documents”.

{-- Example design document
    {
        "language": "javascript",
        "views": {
           "all": {
                "map": "function(doc) { emit(doc.title, doc) }",
            },
            "by_title": {
                "map": "function(doc) { if (doc.title != null) emit(doc.title, doc) }",
            }
        },
        "shows": {
            "recipe": "function(doc, req) { return '<h1>' + doc.title + '</h1>' }"
    }
--}

create :: String -> String -> RequestBuilder
create database designDoc =
    RequestBuilder PUT path Nothing Nothing
        where path = foldPaths [ database
                               , "_design"
                               , designDoc
                               ]

get :: String -> String -> RequestBuilder
get database designDoc =
    RequestBuilder GET path Nothing Nothing
        where path = foldPaths [ database
                               , "_design"
                               , designDoc
                               ]
