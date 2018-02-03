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
import Data.String
import Data.Text (Text)
import Network.Bugsnag.Event
import Network.Bugsnag.ReleaseStage
import Network.HTTP.Client
import Network.HTTP.Client.TLS

newtype BugsnagApiKey = BugsnagApiKey
    { unBugsnagApiKey :: Text
    }
    deriving IsString

data BugsnagSettings m = BugsnagSettings
    { bsApiKey :: BugsnagApiKey
    , bsReleaseStage :: BugsnagReleaseStage
    , bsNotifyReleaseStages :: [BugsnagReleaseStage]
    , bsBeforeNotify :: BugsnagEvent -> m BugsnagEvent
    , bsHttpManager :: Manager
    }

-- | Construct settings purely, given an existing @'Manager'@
bugsnagSettings :: Applicative m => Text -> Manager -> BugsnagSettings m
bugsnagSettings apiKey manager = BugsnagSettings
    { bsApiKey = BugsnagApiKey apiKey
    , bsReleaseStage = ProductionReleaseStage
    , bsNotifyReleaseStages = [ProductionReleaseStage]
    , bsBeforeNotify = pure
    , bsHttpManager = manager
    }

-- | Should the @'BugsnagReleaseStage'@ should trigger notifications?
bugsnagShouldNotify :: BugsnagSettings m -> Bool
bugsnagShouldNotify settings =
    bsReleaseStage settings `elem`
    bsNotifyReleaseStages settings

-- | Construct settings with a new, TLS-enabled @'Manager'@
newBugsnagSettings :: MonadIO m => Text -> m (BugsnagSettings m)
newBugsnagSettings apiKey = bugsnagSettings apiKey <$> newTlsManager
