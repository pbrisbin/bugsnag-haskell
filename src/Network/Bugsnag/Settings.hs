{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
--
-- <https://docs.bugsnag.com/api/error-reporting/#application-settings>
--
module Network.Bugsnag.Settings
    ( BugsnagApiKey(..)
    , BugsnagReleaseStage(..)
    , BugsnagSettings(..)
    , newBugsnagSettings
    , bugsnagSettings
    , shouldNotify
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.String
import Data.Text (Text)
import Network.HTTP.Client
import Network.HTTP.Client.TLS

newtype BugsnagApiKey = BugsnagApiKey
    { unBugsnagApiKey :: Text
    }
    deriving IsString

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

data BugsnagSettings = BugsnagSettings
    { bsApiKey :: BugsnagApiKey
    , bsReleaseStage :: BugsnagReleaseStage
    , bsNotifyReleaseStages :: [BugsnagReleaseStage]
    , bsHttpManager :: Manager
    }

-- | Construct settings purely given an existing @'Manager'@
bugsnagSettings :: Text -> Manager -> BugsnagSettings
bugsnagSettings apiKey manager = BugsnagSettings
    { bsApiKey = BugsnagApiKey apiKey
    , bsReleaseStage = ProductionReleaseStage
    , bsNotifyReleaseStages = [ProductionReleaseStage]
    , bsHttpManager = manager
    }

-- | Construct settings with a new, TLS-enabled @'Manager'@
newBugsnagSettings :: MonadIO m => Text -> m BugsnagSettings
newBugsnagSettings = newBugsnagSettingsWith id

-- | Allow modifying the constructed settings with a function
newBugsnagSettingsWith :: MonadIO m
    => (BugsnagSettings -> BugsnagSettings) -> Text -> m BugsnagSettings
newBugsnagSettingsWith f apiKey = f . bugsnagSettings apiKey <$> newTlsManager

shouldNotify :: BugsnagSettings -> Bool
shouldNotify settings =
    bsReleaseStage settings `elem` bsNotifyReleaseStages settings
