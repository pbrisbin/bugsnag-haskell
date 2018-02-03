{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.BugsnagSpec
    ( spec
    ) where

import Test.Hspec

import Control.Exception
import Network.Bugsnag

brokenFunction :: IO a
brokenFunction = throwBugsnag
    "IOException"
    "Something exploded"
    "brokenFunction"
    $(currentStackFrame)

brokenPureFunction :: HasCallStack => a
brokenPureFunction = brokenHead [] `seq` undefined

brokenHead :: HasCallStack => [a] -> a
brokenHead (x:_) = x
brokenHead _ = error "empty list"

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
            bsfLineNumber frame `shouldBe` 19
            bsfColumnNumber frame `shouldBe` Just 7
            bsfMethod frame `shouldBe` "brokenFunction"
            bsfInProject frame `shouldBe` Just True

    describe "parseBugsnagException" $ do
        it "can parse errors with callstacks" $ do
            e <- evaluate brokenPureFunction `catch` pure

            let ex = bugsnagExceptionFromErrorCall e
            beErrorClass ex `shouldBe` "ErrorCall"
            beMessage ex `shouldBe` Just "empty list"
            beStacktrace ex `shouldSatisfy` ((== 3) . length)

            let frame = head $ beStacktrace ex
            bsfFile frame `shouldBe` "test/Network/BugsnagSpec.hs"
            bsfLineNumber frame `shouldBe` 26
            bsfColumnNumber frame `shouldBe` Just 16
            bsfMethod frame `shouldBe` "error"

            map bsfMethod (beStacktrace ex)
                `shouldBe` ["error", "brokenHead", "brokenPureFunction"]
