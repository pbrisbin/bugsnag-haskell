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
import qualified Data.Text as T
import Network.Bugsnag.BeforeNotify
import Network.Bugsnag.Event
import Network.Bugsnag.Exception
import Network.Bugsnag.ReleaseStage
import Network.Bugsnag.StackFrame
import Network.HTTP.Client
import Network.HTTP.Client.TLS

newtype BugsnagApiKey = BugsnagApiKey
    { unBugsnagApiKey :: Text
    }
    deriving (FromJSON, IsString)

instance Show BugsnagApiKey where
    show = T.unpack . unBugsnagApiKey

-- | Notifier settings
--
-- See @'newBugsnagSettings'@.
--
data BugsnagSettings = BugsnagSettings
    { bsApiKey :: BugsnagApiKey
    -- ^ Your Integration API Key.
    , bsAppVersion :: Maybe Text
    -- ^ The version of your application
    --
    -- Marking bugs as Fixed and having them auto-reopen in new versions
    -- requires you set this.
    --
    , bsReleaseStage :: BugsnagReleaseStage
    -- ^ The current release-stage, Production by default
    , bsNotifyReleaseStages :: [BugsnagReleaseStage]
    -- ^ Which release-stages to notify in. Only Production by default
    , bsBeforeNotify :: BeforeNotify
    -- ^ Modify any events before they are sent
    --
    -- For example to attach a user, or set the context.
    --
    , bsIgnoreException :: BugsnagException -> Bool
    -- ^ Exception filtering
    --
    -- Functions like @'notifyBugsnag'@ will do nothing with exceptions that
    -- pass this predicate. N.B. Something lower-level, like @'reportError'@
    -- won't be aware of this.
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
    -- ^ Stack frame filter
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
    , bsIgnoreException = const False
    , bsGroupingHash = const Nothing
    , bsIsInProject = const True
    , bsFilterStackFrames = const True
    , bsHttpManager = manager
    }

-- | Should this @'BugsnagException'@ trigger notification?
--
-- >>> :set -XOverloadedStrings
-- >>> settings <- newBugsnagSettings ""
-- >>> let exception = bugsnagException "" "" []
-- >>> bugsnagShouldNotify settings exception
-- True
--
-- >>> let devSettings = settings { bsReleaseStage = DevelopmentReleaseStage }
-- >>> bugsnagShouldNotify devSettings exception
-- False
--
-- >>> bugsnagShouldNotify devSettings { bsNotifyReleaseStages = [DevelopmentReleaseStage] } exception
-- True
--
-- >>> let ignore = (== "IgnoreMe") . beErrorClass
-- >>> let ignoreSettings = settings { bsIgnoreException = ignore }
-- >>> bugsnagShouldNotify ignoreSettings exception
-- True
--
-- >>> bugsnagShouldNotify ignoreSettings exception { beErrorClass = "IgnoreMe" }
-- False
--
bugsnagShouldNotify :: BugsnagSettings -> BugsnagException -> Bool
bugsnagShouldNotify settings exception
    | bsReleaseStage settings `notElem` bsNotifyReleaseStages settings = False
    | bsIgnoreException settings exception = False
    | otherwise = True

-- | Construct settings with a new, TLS-enabled @'Manager'@
--
-- >>> :set -XOverloadedStrings
-- >>> settings <- newBugsnagSettings "API_KEY"
-- >>> bsApiKey settings
-- API_KEY
--
-- >>> bsReleaseStage settings
-- ProductionReleaseStage
--
-- >>> bsNotifyReleaseStages settings
-- [ProductionReleaseStage]
--
newBugsnagSettings :: BugsnagApiKey -> IO BugsnagSettings
newBugsnagSettings apiKey = bugsnagSettings apiKey <$> newTlsManager
