{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.BugsnagSpec
    ( spec
    ) where

import Test.Hspec

import Control.Exception (catch)
import Network.Bugsnag
import Network.Bugsnag.Throw

brokenFunction :: IO a
brokenFunction = throwBugsnag
    "IOException"
    "Something exploded"
    "brokenFunction"
    $(currentStackFrame)

spec :: Spec
spec = do
    describe "throwBugsnag" $ do
        it "includes location of the splice as a stack frame" $ do
            ex <- brokenFunction `catch` pure

            beErrorClass ex `shouldBe` "IOException"
            beMessage ex `shouldBe` Just "Something exploded"
            beStacktrace ex `shouldSatisfy` (not . null)

            let frame = head $ beStacktrace ex
            bsfFile frame `shouldBe` "test/Network/BugsnagSpec.hs"
            bsfLineNumber frame `shouldBe` 18
            bsfColumnNumber frame `shouldBe` Just 7
            bsfMethod frame `shouldBe` "brokenFunction"
            bsfInProject frame `shouldBe` Just True
