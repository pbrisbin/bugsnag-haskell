module Network.Bugsnag.ExceptionSpec
  ( spec
  ) where

import           Prelude

import           Control.Exception
import           Data.Bugsnag
import           Examples
import           Network.Bugsnag.Exception
import           Test.Hspec

spec :: Spec
spec = do
  describe "AsException" $ do
    it "can throw and catch a Bugsnag.Exception" $ do
      AsException ex <- brokenFunctionIO `catch` pure

      exception_errorClass ex `shouldBe` "IOException"
      exception_message ex `shouldBe` Just "Something exploded"
      exception_stacktrace ex `shouldSatisfy` (not . null)

      let frame = head $ exception_stacktrace ex
      stackFrame_file frame `shouldBe` "test/Examples.hs"
      stackFrame_lineNumber frame `shouldBe` 27
      -- different versions of GHC disagree on where splices start
      stackFrame_columnNumber frame
        `shouldSatisfy` (`elem` [Just 36, Just 37])
      stackFrame_method frame `shouldBe` "brokenFunctionIO"
      stackFrame_inProject frame `shouldBe` Just True

    describe "bugsnagExceptionFromSomeException" $ do
      it "sets errorClass" $ do
        let ex =
              bugsnagExceptionFromSomeException $
                toException $
                  userError "Oops"

        exception_errorClass ex `shouldBe` "IOException"
        exception_message ex `shouldBe` Just "user error (Oops)"

      it "can parse errors with callstacks" $ do
        e <- evaluate brokenFunction `catch` pure

        let ex = bugsnagExceptionFromSomeException e
        exception_errorClass ex `shouldBe` "ErrorCall"
        exception_message ex `shouldBe` Just "empty list"
        exception_stacktrace ex `shouldSatisfy` ((== 3) . length)

        let frame = head $ exception_stacktrace ex
        stackFrame_file frame `shouldBe` "test/Examples.hs"
        stackFrame_lineNumber frame `shouldBe` 35
        stackFrame_columnNumber frame `shouldBe` Just 15
        stackFrame_method frame `shouldBe` "error"

        map stackFrame_method (exception_stacktrace ex)
          `shouldBe` ["error", "sillyHead", "brokenFunction"]

      it "also parses StringException" $ do
        e <- brokenFunction' `catch` pure

        let ex = bugsnagExceptionFromSomeException e
        exception_errorClass ex `shouldBe` "StringException"
        exception_message ex `shouldBe` Just "empty list"
        exception_stacktrace ex `shouldSatisfy` ((== 3) . length)

        let frame = head $ exception_stacktrace ex
        stackFrame_file frame `shouldBe` "test/Examples.hs"
        stackFrame_lineNumber frame `shouldBe` 42
        stackFrame_columnNumber frame `shouldBe` Just 16
        stackFrame_method frame `shouldBe` "throwString"

        map stackFrame_method (exception_stacktrace ex)
          `shouldBe` ["throwString", "sillyHead'", "brokenFunction'"]

      it "also parses StringExceptions with newlines" $ do
        e <- brokenFunction'' `catch` pure

        let ex = bugsnagExceptionFromSomeException e
        exception_errorClass ex `shouldBe` "StringException"
        exception_message ex
          `shouldBe` Just
            "empty list\n and message with newlines\n\n"
        exception_stacktrace ex `shouldSatisfy` ((== 3) . length)

        let frame = head $ exception_stacktrace ex
        stackFrame_file frame `shouldBe` "test/Examples.hs"
        stackFrame_lineNumber frame `shouldBe` 49
        stackFrame_columnNumber frame `shouldBe` Just 17
        stackFrame_method frame `shouldBe` "throwString"

        map stackFrame_method (exception_stacktrace ex)
          `shouldBe` [ "throwString"
                     , "sillyHead''"
                     , "brokenFunction''"
                     ]
