module ApiSpec ( main ) where

import           Test.Hspec

import qualified Network.Cloudant.Api as Cloudant

main :: IO ()
main = hspec spec

spec :: Spec
spec = hspec $ do
  describe "The Cloudant API" $ do
    it "should work" $ do
      head [23 ..] `shouldBe` (23 :: Int)
