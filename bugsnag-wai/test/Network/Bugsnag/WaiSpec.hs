module Network.Bugsnag.WaiSpec
  ( spec
  ) where

import Prelude

import qualified Control.Exception as Exception
import Data.Bugsnag
import qualified Data.HashMap.Strict as HashMap
import Network.Bugsnag
import Network.Bugsnag.Wai
import Test.Hspec

data TestException = TestException
  deriving stock (Show)
  deriving anyclass (Exception.Exception)

spec :: Spec
spec = do
  describe "redactRequestHeaders" $ do
    it "redacts the given headers" $ do
      let
        bn = redactRequestHeaders ["Authorization"]

        event =
          runBeforeNotify bn TestException $
            defaultEvent
              { event_request =
                  Just
                    defaultRequest
                      { request_headers =
                          Just $
                            HashMap.fromList
                              [("Authorization", "secret"), ("X-Foo", "Bar")]
                      }
              }

        lookupEventRequestHeader k e = do
          r <- event_request e
          hs <- request_headers r
          HashMap.lookup k hs

      lookupEventRequestHeader "Authorization" event
        `shouldBe` Just "<redacted>"
      lookupEventRequestHeader "X-Foo" event `shouldBe` Just "Bar"

  describe "readForwardedFor" $ do
    it "handles empty" $ do
      readForwardedFor "" `shouldBe` Nothing

    it "reads a single value" $ do
      readForwardedFor "123.123.123" `shouldBe` Just "123.123.123"

    it "reads the first of many values" $ do
      readForwardedFor "123.123.123, 45.45.45"
        `shouldBe` Just "123.123.123"
