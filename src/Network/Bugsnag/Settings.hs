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

import Data.Aeson (FromJSON)
import Data.String
import Data.Text (Text)
import Data.Version
import Network.Bugsnag.Event
import Network.Bugsnag.ReleaseStage
import Network.Bugsnag.StackFrame
import Network.HTTP.Client
import Network.HTTP.Client.TLS

newtype BugsnagApiKey = BugsnagApiKey
    { unBugsnagApiKey :: Text
    }
    deriving (FromJSON, IsString)

-- | Notifier settings
--
-- See @'newBugsnagSettings'@.
--
data BugsnagSettings = BugsnagSettings
    { bsApiKey :: BugsnagApiKey
    -- ^ Your Integration API Key.
    , bsAppVersion :: Maybe Version
    -- ^ The version of your application
    --
    -- Marking bugs as Fixed and having them auto-reopen in new versions
    -- requires you set this.
    --
    , bsReleaseStage :: BugsnagReleaseStage
    -- ^ The current release-stage (Production, Development, etc)
    , bsNotifyReleaseStages :: [BugsnagReleaseStage]
    -- ^ Which release-stages to notify in. By default Production and Staging.
    , bsBeforeNotify :: BugsnagEvent -> BugsnagEvent
    -- ^ Modify any events before they are sent
    --
    -- For example to attach a user, or set the context.
    --
    , bsGroupingHash :: BugsnagEvent -> Maybe Text
    -- ^ The grouping hash to use for any specific event
    --
    -- Events (exceptions) which have the same hash will be counted as the same
    -- exception. Use @Nothing@ (the default) to maintain Bugsnags automatic
    -- behavior (group by top in-project stack-frame).
    --
    , bsIsInProject :: FilePath -> Bool
    -- ^ Predicate for in-project stack frames
    --
    -- By default, all are considered in-project.
    --
    , bsFilterStackFrames :: BugsnagStackFrame -> Bool
    -- ^ Stack-fram filter
    --
    -- Return @True@ for any stack-frames that should be omitted from
    -- notifications.
    --
    , bsHttpManager :: Manager
    -- ^ The HTTP @Manager@ used to emit notifications
    --
    -- It's more efficient, and ensures proper resource cleanup, to share a
    -- single manager across an application. Must be TLS-enabled.
    --
    }

-- | Construct settings purely, given an existing @'Manager'@
bugsnagSettings :: BugsnagApiKey -> Manager -> BugsnagSettings
bugsnagSettings apiKey manager = BugsnagSettings
    { bsApiKey = apiKey
    , bsAppVersion = Nothing
    , bsReleaseStage = ProductionReleaseStage
    , bsNotifyReleaseStages = [ProductionReleaseStage]
    , bsBeforeNotify = id
    , bsGroupingHash = const Nothing
    , bsIsInProject = const True
    , bsFilterStackFrames = const True
    , bsHttpManager = manager
    }

-- | Should the @'BugsnagReleaseStage'@ should trigger notifications?
bugsnagShouldNotify :: BugsnagSettings -> Bool
bugsnagShouldNotify settings =
    bsReleaseStage settings `elem`
    bsNotifyReleaseStages settings

-- | Construct settings with a new, TLS-enabled @'Manager'@
newBugsnagSettings :: BugsnagApiKey -> IO BugsnagSettings
newBugsnagSettings apiKey = bugsnagSettings apiKey <$> newTlsManager
