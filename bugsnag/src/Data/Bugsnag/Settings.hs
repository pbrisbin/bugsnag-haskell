module Data.Bugsnag.Settings
  ( Settings (..)
  , defaultSettings
  ) where

import           Prelude

import           Data.Bugsnag
import           Data.Text                    (Text)
import           Network.Bugsnag.BeforeNotify
import           Network.Bugsnag.CodeIndex
import           Network.HTTP.Client          (HttpException)

data Settings = Settings
  { settings_apiKey               :: ApiKey
  -- ^ Your Integration API Key
  , settings_appVersion           :: Maybe Text
  -- ^ The version of your application
  --
  -- Marking bugs as Fixed and having them auto-reopen in new versions
  -- requires you set this.
  , settings_releaseStage         :: Text
  -- ^ The current release-stage, Production by default
  , settings_enabledReleaseStages :: [Text]
  -- ^ Which release-stages to notify in. Only Production by default
  , settings_beforeNotify         :: BeforeNotify
  -- ^ Modify any events before they are sent
  --
  -- For example to attach a user, or set the context.
  , settings_ignoreException      :: Exception -> Bool
  -- ^ Exception filtering
  --
  -- Functions like 'notifyBugsnag' will do nothing with exceptions that pass
  -- this predicate. N.B. Something lower-level, like 'reportError' won't be
  -- aware of this.
  , settings_onNotifyException    :: HttpException -> IO ()
  -- ^ How to handle an exception reporting error events
  --
  -- Default is to ignore.
  , settings_codeIndex            :: Maybe CodeIndex
  -- ^ A 'CodeIndex' built at compile-time from project sources
  --
  -- If set, this will be used to update StackFrames to include lines of
  -- source code context as read out of this value. N.B. using this means
  -- loading and keeping the source code for the entire project in memory.
  }

defaultSettings :: Text -> Settings
defaultSettings k =
  Settings
    { settings_apiKey = apiKey k
    , settings_appVersion = Nothing
    , settings_releaseStage = "production"
    , settings_enabledReleaseStages = ["production"]
    , settings_beforeNotify = mempty
    , settings_ignoreException = const False
    , settings_onNotifyException = const $ pure ()
    , settings_codeIndex = Nothing
    }
