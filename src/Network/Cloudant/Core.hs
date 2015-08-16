module Network.Cloudant.Core where

import qualified Network.Cloudant.Database         as Database
import           Network.Cloudant.Internal.Request (runRequest)
import           Network.Cloudant.Internal.Types   (Config (..))
import qualified Network.Cloudant.Transform        as TF

getDatabases :: Config -> IO (Either String (Maybe [String]))
getDatabases config = TF.transform response :: IO (Either String (Maybe [String]))
    where response = runRequest config (Database.getAll)
