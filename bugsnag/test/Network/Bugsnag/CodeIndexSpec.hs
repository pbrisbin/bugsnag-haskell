{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-local-signatures #-}

module Network.Bugsnag.CodeIndexSpec
    ( spec
    ) where

import Prelude

import Network.Bugsnag.CodeIndex
import Test.Hspec

spec :: Spec
spec = do
    describe "CodeIndex" $ do
        let index = $(buildCodeIndex "test/fixtures/index-project/**/*.hs")

        it "can find ranges within the file" $ do
            let path = "test/fixtures/index-project/Foo.hs"
                range = (0, 3)
                sourceLines =
                    [ (0, "module Foo where")
                    , (1, "")
                    , (2, "data What = What")
                    , (3, "    deriving Show")
                    ]

            findSourceRange path range index `shouldBe` Just sourceLines

        it "handles ranges that extend beyond bounds" $ do
            let path = "test/fixtures/index-project/Foo.hs"
                range = (6, 10)
                sourceLines =
                    [(6, "what 0 = Just What"), (7, "what _ = Nothing")]

            findSourceRange path range index `shouldBe` Just sourceLines

        it "handles ranges that are totally beyond bounds" $ do
            let path = "test/fixtures/index-project/Foo.hs"
                range = (10, 12)

            findSourceRange path range index `shouldBe` Just []

        it "handles missing files" $ do
            findSourceRange "nope.hs" (0, 3) index `shouldBe` Nothing
