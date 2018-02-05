{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
--
-- <https://docs.bugsnag.com/api/error-reporting/#application-settings>
--
module Network.Bugsnag.Settings
    ( BugsnagApiKey(..)
    , BugsnagSettings(..)
    , newBugsnagSettings
    , bugsnagSettings
    , bugsnagShouldNotify
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON)
import Data.String
import Data.Text (Text)
import Network.Bugsnag.Event
import Network.Bugsnag.ReleaseStage
import Network.Bugsnag.StackFrame
import Network.HTTP.Client
import Network.HTTP.Client.TLS

newtype BugsnagApiKey = BugsnagApiKey
    { unBugsnagApiKey :: Text
    }
    deriving (FromJSON, IsString)

data BugsnagSettings m = BugsnagSettings
    { bsApiKey :: BugsnagApiKey
    , bsReleaseStage :: BugsnagReleaseStage
    , bsNotifyReleaseStages :: [BugsnagReleaseStage]
    , bsBeforeNotify :: BugsnagEvent -> m BugsnagEvent
    , bsGroupingHash :: BugsnagEvent -> Maybe Text
    , bsIsInProject :: FilePath -> Bool
    , bsFilterStackFrames :: BugsnagStackFrame -> Bool
    , bsHttpManager :: Manager
    }

-- | Construct settings purely, given an existing @'Manager'@
bugsnagSettings :: Applicative m => BugsnagApiKey -> Manager -> BugsnagSettings m
bugsnagSettings apiKey manager = BugsnagSettings
    { bsApiKey = apiKey
    , bsReleaseStage = ProductionReleaseStage
    , bsNotifyReleaseStages = [ProductionReleaseStage]
    , bsBeforeNotify = pure
    , bsGroupingHash = const Nothing
    , bsIsInProject = const True
    , bsFilterStackFrames = const True
    , bsHttpManager = manager
    }

-- | Should the @'BugsnagReleaseStage'@ should trigger notifications?
bugsnagShouldNotify :: BugsnagSettings m -> Bool
bugsnagShouldNotify settings =
    bsReleaseStage settings `elem`
    bsNotifyReleaseStages settings

-- | Construct settings with a new, TLS-enabled @'Manager'@
newBugsnagSettings :: MonadIO m => BugsnagApiKey -> m (BugsnagSettings m)
newBugsnagSettings apiKey = bugsnagSettings apiKey <$> newTlsManager
