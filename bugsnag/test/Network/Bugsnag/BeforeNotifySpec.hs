module Network.Bugsnag.BeforeNotifySpec
    ( spec
    ) where


import Prelude

import Control.Exception
import Data.Bugsnag hiding (Exception)
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
