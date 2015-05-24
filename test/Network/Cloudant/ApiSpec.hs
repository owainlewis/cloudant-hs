module Network.Cloudant.ApiSpec ( main, spec ) where

import           Network.Cloudant.Api
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "URL Helpers" $ do
    it "should return the correct account url" $ do
      let underTest = urlForAccount "foo"
      underTest `shouldBe` "https://foo.cloudant.com"

    it "should return the correct resource" $ do
      let underTest = getHTTPEndpoint "foo" "/bar"
      underTest `shouldBe` "https://foo.cloudant.com/bar"

  describe "Databases" $ do
    it "should return the createDatabase resource" $ do
      let underTest = (CreateDatabase "foo" "users")
      (getResource underTest) `shouldBe` "https://foo.cloudant.com/users"

    it "should return the getDatabases resource" $ do
      let underTest = (GetDatabases "foo")
      (getResource underTest) `shouldBe` "https://foo.cloudant.com/_all_dbs"
