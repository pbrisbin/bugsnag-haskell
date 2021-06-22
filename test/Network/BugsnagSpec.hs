{-# LANGUAGE TemplateHaskell #-}

module Network.BugsnagSpec
    ( spec
    )
where

import Prelude

import Control.Exception
import Network.Bugsnag
import Test.Hspec
import UnliftIO.Exception (throwString)

brokenFunctionIO :: IO a
brokenFunctionIO = throw $ bugsnagException
    "IOException"
    "Something exploded"
    [$(currentStackFrame) "brokenFunctionIO"]

brokenFunction :: HasCallStack => a
brokenFunction = sillyHead [] `seq` undefined

sillyHead :: HasCallStack => [a] -> a
sillyHead (x : _) = x
sillyHead _ = error "empty list"

brokenFunction' :: HasCallStack => IO a
brokenFunction' = sillyHead' []

sillyHead' :: HasCallStack => [a] -> IO a
sillyHead' (x : _) = pure x
sillyHead' _ = throwString "empty list"

brokenFunction'' :: HasCallStack => IO a
brokenFunction'' = sillyHead'' []

sillyHead'' :: HasCallStack => [a] -> IO a
sillyHead'' (x : _) = pure x
sillyHead'' _ = throwString "empty list\n and message with newlines\n\n"

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
            bsfLineNumber frame `shouldBe` 19
            -- different versions of GHC disagree, apparently, if the column is
            -- the opening of the splice (7) or the first character into the
            -- splice (8)
            --bsfColumnNumber frame `shouldBe` Just 8
            bsfMethod frame `shouldBe` "brokenFunctionIO"
            bsfInProject frame `shouldBe` Just True

        describe "bugsnagExceptionFromSomeException" $ do
            it "can parse errors with callstacks" $ do
                e <- evaluate brokenFunction `catch` pure

                let ex = bugsnagExceptionFromSomeException e
                beErrorClass ex `shouldBe` "ErrorCall"
                beMessage ex `shouldBe` Just "empty list"
                beStacktrace ex `shouldSatisfy` ((== 3) . length)

                let frame = head $ beStacktrace ex
                bsfFile frame `shouldBe` "test/Network/BugsnagSpec.hs"
                bsfLineNumber frame `shouldBe` 26
                bsfColumnNumber frame `shouldBe` Just 15
                bsfMethod frame `shouldBe` "error"

                map bsfMethod (beStacktrace ex)
                    `shouldBe` ["error", "sillyHead", "brokenFunction"]

            it "also parses StringException" $ do
                e <- brokenFunction' `catch` pure

                let ex = bugsnagExceptionFromSomeException e
                beErrorClass ex `shouldBe` "StringException"
                beMessage ex `shouldBe` Just "empty list"
                beStacktrace ex `shouldSatisfy` ((== 3) . length)

                let frame = head $ beStacktrace ex
                bsfFile frame `shouldBe` "test/Network/BugsnagSpec.hs"
                bsfLineNumber frame `shouldBe` 33
                bsfColumnNumber frame `shouldBe` Just 16
                bsfMethod frame `shouldBe` "throwString"

                map bsfMethod (beStacktrace ex)
                    `shouldBe` ["throwString", "sillyHead'", "brokenFunction'"]

            it "also parses StringExceptions with newlines" $ do
                e <- brokenFunction'' `catch` pure

                let ex = bugsnagExceptionFromSomeException e
                beErrorClass ex `shouldBe` "StringException"
                beMessage ex `shouldBe` Just
                    "empty list\n and message with newlines\n\n"
                beStacktrace ex `shouldSatisfy` ((== 3) . length)

                let frame = head $ beStacktrace ex
                bsfFile frame `shouldBe` "test/Network/BugsnagSpec.hs"
                bsfLineNumber frame `shouldBe` 40
                bsfColumnNumber frame `shouldBe` Just 17
                bsfMethod frame `shouldBe` "throwString"

                map bsfMethod (beStacktrace ex)
                    `shouldBe` [ "throwString"
                               , "sillyHead''"
                               , "brokenFunction''"
                               ]
