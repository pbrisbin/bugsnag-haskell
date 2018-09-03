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
import Network.Bugsnag.CodeIndex
import Network.Bugsnag.Event
import Network.Bugsnag.Exception
import Network.Bugsnag.ReleaseStage
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
    -- For example to attach a user, or set the context. By default, we use
    -- @'redactRequestHeaders'@ to strip some sensitive Headers from the
    -- Request.
    --
    , bsIgnoreException :: BugsnagException -> Bool
    -- ^ Exception filtering
    --
    -- Functions like @'notifyBugsnag'@ will do nothing with exceptions that
    -- pass this predicate. N.B. Something lower-level, like @'reportError'@
    -- won't be aware of this.
    --
    , bsHttpManager :: Manager
    -- ^ The HTTP @Manager@ used to emit notifications
    --
    -- It's more efficient, and ensures proper resource cleanup, to share a
    -- single manager across an application. Must be TLS-enabled.
    --
    , bsCodeIndex :: Maybe CodeIndex
    -- ^ A @'CodeIndex'@ built at compile-time from project sources
    --
    -- If set, this will be used to update StackFrames to include lines of
    -- source code context as read out of this value. N.B. using this means
    -- loading and keeping the source code for the entire project in memory.
    --
    }

-- | Construct settings purely, given an existing @'Manager'@
bugsnagSettings :: BugsnagApiKey -> Manager -> BugsnagSettings
bugsnagSettings apiKey manager = BugsnagSettings
    { bsApiKey = apiKey
    , bsAppVersion = Nothing
    , bsReleaseStage = ProductionReleaseStage
    , bsNotifyReleaseStages = [ProductionReleaseStage]
    , bsBeforeNotify = defaultBeforeNotify
    , bsIgnoreException = const False
    , bsHttpManager = manager
    , bsCodeIndex = Nothing
    }

-- | Should this @'BugsnagEvent'@ trigger notification?
--
-- >>> :set -XOverloadedStrings
-- >>> settings <- newBugsnagSettings ""
-- >>> let event = bugsnagEvent $ bugsnagException "" "" []
-- >>> bugsnagShouldNotify settings event
-- True
--
-- >>> let devSettings = settings { bsReleaseStage = DevelopmentReleaseStage }
-- >>> bugsnagShouldNotify devSettings event
-- False
--
-- >>> bugsnagShouldNotify devSettings { bsNotifyReleaseStages = [DevelopmentReleaseStage] } event
-- True
--
-- >>> let ignore = (== "IgnoreMe") . beErrorClass
-- >>> let ignoreSettings = settings { bsIgnoreException = ignore }
-- >>> let ignoreEvent = bugsnagEvent $ bugsnagException "IgnoreMe" "" []
-- >>> bugsnagShouldNotify ignoreSettings event
-- True
--
-- >>> bugsnagShouldNotify ignoreSettings ignoreEvent
-- False
--
bugsnagShouldNotify :: BugsnagSettings -> BugsnagEvent -> Bool
bugsnagShouldNotify settings event
    | bsReleaseStage settings `notElem` bsNotifyReleaseStages settings = False
    | bsIgnoreException settings $ beException event = False
    | otherwise = True

-- | Construct settings with a new, TLS-enabled @'Manager'@
--
-- Uses @'getGlobalManager'@.
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
newBugsnagSettings apiKey = bugsnagSettings apiKey <$> getGlobalManager
