module Network.Bugsnag.ReleaseStage
    ( BugsnagReleaseStage(..)
    )
where

import Prelude

import Data.Aeson
import Data.Text (Text)

data BugsnagReleaseStage
    = DevelopmentReleaseStage
    | StagingReleaseStage
    | ProductionReleaseStage
    | CustomReleaseStage Text
    deriving stock (Eq, Show)

instance FromJSON BugsnagReleaseStage where
    parseJSON = withText "ReleaseStage" $ \case
        "development" -> pure DevelopmentReleaseStage
        "staging" -> pure StagingReleaseStage
        "production" -> pure ProductionReleaseStage
        t -> pure $ CustomReleaseStage t

instance ToJSON BugsnagReleaseStage where
    toJSON DevelopmentReleaseStage = String "development"
    toJSON StagingReleaseStage = String "staging"
    toJSON ProductionReleaseStage = String "production"
    toJSON (CustomReleaseStage t) = String t
