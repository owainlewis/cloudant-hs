module Network.Cloudant.CoreSpec ( main, spec ) where

import           Network.Cloudant.Core
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Network.Cloudant.Core" $ do
        it "work" $ do
            1 `shouldBe` 1 :: Int
