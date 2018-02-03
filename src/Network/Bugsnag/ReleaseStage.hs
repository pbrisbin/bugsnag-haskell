{-# LANGUAGE OverloadedStrings #-}
module Network.Bugsnag.ReleaseStage
    ( BugsnagReleaseStage(..)
    ) where


import Data.Aeson
import Data.Text (Text)

data BugsnagReleaseStage
    = DevelopmentReleaseStage
    | StagingReleaseStage
    | ProductionReleaseStage
    | CustomReleaseStage Text
    deriving Eq

instance ToJSON BugsnagReleaseStage where
    toJSON DevelopmentReleaseStage = String "development"
    toJSON StagingReleaseStage = String "staging"
    toJSON ProductionReleaseStage = String "production"
    toJSON (CustomReleaseStage t) = String t
