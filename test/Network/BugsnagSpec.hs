{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.BugsnagSpec
    ( spec
    ) where

import Test.Hspec

import Control.Exception (catch, throwIO)
import Network.Bugsnag

brokenFunction :: IO a
brokenFunction = throwIO $ $(bugsnagException) "brokenFunction" "Oops"

spec :: Spec
spec = do
    describe "bugsnagException" $ do
        it "includes location of the splice as a stack frame" $ do
            ex <- brokenFunction `catch` pure

            beErrorClass ex `shouldBe` "brokenFunction"
            beMessage ex `shouldBe` Just "Oops"
            beStacktrace ex `shouldSatisfy` (not . null)

            let frame = head $ beStacktrace ex
            bsfFile frame `shouldBe` "test/Network/BugsnagSpec.hs"
            bsfLineNumber frame `shouldBe` 13
            bsfColumnNumber frame `shouldBe` Just 30
            bsfMethod frame `shouldBe` "Network.BugsnagSpec"
