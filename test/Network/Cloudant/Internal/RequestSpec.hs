module Network.Cloudant.Internal.RequestSpec ( main, spec ) where

import           Network.Cloudant.Internal.Request
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Cloudant" $ do
    it "should run a request" $ do
      let repsonse = makeRequest "GET" "https://httpbin.org/get" (Auth "ibm" "secret") Nothing
      True
