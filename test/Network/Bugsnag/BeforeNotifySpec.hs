module Network.Bugsnag.BeforeNotifySpec
    ( spec
    ) where


import Prelude

import Control.Exception
import Data.Bugsnag hiding (Exception)
import qualified Data.HashMap.Strict as HashMap
import Network.Bugsnag.BeforeNotify
import Test.Hspec

data FooException = FooException
    deriving stock Show
    deriving anyclass Exception

data BarException = BarException
    deriving stock Show
    deriving anyclass Exception

data BazException = BazException
    deriving stock Show
    deriving anyclass Exception

spec :: Spec
spec = do
    describe "updateEventFromOriginalException" $ do
        it "can update based on unknown exception types" $ do
            let asFoo FooException = setGroupingHash "Saw Foo"
                asBar BarException = setGroupingHash "Saw Bar"

                bn = mconcat
                    [ updateEventFromOriginalException asFoo
                    , updateEventFromOriginalException asBar
                    ]

            event_groupingHash (runBeforeNotify bn FooException defaultEvent)
                `shouldBe` Just "Saw Foo"

            event_groupingHash (runBeforeNotify bn BarException defaultEvent)
                `shouldBe` Just "Saw Bar"

            event_groupingHash (runBeforeNotify bn BazException defaultEvent)
                `shouldBe` Nothing

    describe "redactRequestHeaders" $ do
        it "redacts the given headers" $ do
            let bn = redactRequestHeaders ["Authorization"]

                event = runBeforeNotify bn FooException $ defaultEvent
                    { event_request = Just defaultRequest
                        { request_headers =
                            Just $ HashMap.fromList
                                [("Authorization", "secret"), ("X-Foo", "Bar")]
                        }
                    }

                eventRequestHeaders e = do
                    r <- event_request e
                    hs <- request_headers r
                    pure $ HashMap.toList hs

            eventRequestHeaders event `shouldBe` Just
                [("Authorization", "<redacted>"), ("X-Foo", "Bar")]
