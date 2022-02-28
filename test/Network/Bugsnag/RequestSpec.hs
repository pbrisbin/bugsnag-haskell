module Network.Bugsnag.RequestSpec
    ( spec
    ) where

import Prelude

import Network.Bugsnag.Request
import Test.Hspec

spec :: Spec
spec = do
    describe "readForwardedFor" $ do
        it "handles empty" $ do
            readForwardedFor "" `shouldBe` Nothing

        it "reads a single value" $ do
            readForwardedFor "123.123.123" `shouldBe` Just "123.123.123"

        it "reads the first of many values" $ do
            readForwardedFor "123.123.123, 45.45.45"
                `shouldBe` Just "123.123.123"
