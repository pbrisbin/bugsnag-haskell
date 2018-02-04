{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.BugsnagSpec
    ( spec
    ) where

import Test.Hspec

import Control.Exception
import Control.Monad.Catch (throwM)
import Network.Bugsnag

brokenFunctionIO :: IO a
brokenFunctionIO = throwM $ bugsnagException
    "IOException" "Something exploded" [$(currentStackFrame) "brokenFunctionIO"]

brokenFunction :: HasCallStack => a
brokenFunction = sillyHead [] `seq` undefined

sillyHead :: HasCallStack => [a] -> a
sillyHead (x:_) = x
sillyHead _ = error "empty list"

spec :: Spec
spec = do
    describe "BugsnagException" $ do
        it "can be thrown and caught" $ do
            ex <- brokenFunctionIO `catch` pure

            beErrorClass ex `shouldBe` "IOException"
            beMessage ex `shouldBe` Just "Something exploded"
            beStacktrace ex `shouldSatisfy` (not . null)

            let frame = head $ beStacktrace ex
            bsfFile frame `shouldBe` "test/Network/BugsnagSpec.hs"
            bsfLineNumber frame `shouldBe` 17
            bsfColumnNumber frame `shouldBe` Just 43
            bsfMethod frame `shouldBe` "brokenFunctionIO"
            bsfInProject frame `shouldBe` Just True

    describe "parseBugsnagException" $ do
        it "can parse errors with callstacks" $ do
            e <- evaluate brokenFunction `catch` pure

            let ex = bugsnagExceptionFromErrorCall e
            beErrorClass ex `shouldBe` "ErrorCall"
            beMessage ex `shouldBe` Just "empty list"
            beStacktrace ex `shouldSatisfy` ((== 3) . length)

            let frame = head $ beStacktrace ex
            bsfFile frame `shouldBe` "test/Network/BugsnagSpec.hs"
            bsfLineNumber frame `shouldBe` 24
            bsfColumnNumber frame `shouldBe` Just 15
            bsfMethod frame `shouldBe` "error"

            map bsfMethod (beStacktrace ex)
                `shouldBe` ["error", "sillyHead", "brokenFunction"]
