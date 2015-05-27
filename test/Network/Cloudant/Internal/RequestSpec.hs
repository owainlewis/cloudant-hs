module Network.Cloudant.Internal.RequestSpec ( main, spec ) where

import           Network.Cloudant.Internal.Request
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
