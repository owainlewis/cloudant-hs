module Network.Cloudant.ApiSpec ( main, spec ) where

import           Network.Cloudant.Api
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Cloudant" $ do
    it "should get the username" $ do
      let basic = ("foo", "bar")
      (user basic) `shouldBe` "foo"
      (pass basic) `shouldBe` "bar"

  describe "getting an account url" $ do
    it "should return the correct account url" $ do
      let underTest = urlForAccount "foo"
      underTest `shouldBe` "https://foo.cloudant.com"

  describe "getting a HTTP endpoint for a given resource" $ do
    it "should return the correct resource" $ do
      let underTest = getHTTPEndpoint "foo" "/bar"
      underTest `shouldBe` "https://foo.cloudant.com/bar"

  describe "databases" $ do
    it "should return the createDatabase resource" $ do
      let underTest = (CreateDatabase "foo" "users")
      (getResource underTest) `shouldBe` "https://foo.cloudant.com/users"

    it "should return the getDatabases resource" $ do
      let underTest = (GetDatabases "foo")
      (getResource underTest) `shouldBe` "https://foo.cloudant.com/_all_dbs"
