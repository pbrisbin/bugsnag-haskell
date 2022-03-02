module Network.Bugsnag.Notify
    ( notifyBugsnag
    , notifyBugsnagWith
    ) where

import Prelude

import qualified Control.Exception as Exception
import Control.Monad (unless)
import Data.Bugsnag
import Data.Bugsnag.Settings
import Network.Bugsnag.BeforeNotify
import Network.Bugsnag.Exception
import Network.HTTP.Client.TLS (getGlobalManager)

notifyBugsnag :: Exception.Exception e => Settings -> e -> IO ()
notifyBugsnag = notifyBugsnagWith mempty

notifyBugsnagWith
    :: Exception.Exception e => BeforeNotify -> Settings -> e -> IO ()
notifyBugsnagWith f settings = reportEvent settings . buildEvent bn
    where bn = f <> globalBeforeNotify settings

reportEvent :: Settings -> Event -> IO ()
reportEvent Settings {..} event = unless (null $ event_exceptions event) $ do
    m <- getGlobalManager
    result <- sendEvents m settings_apiKey [event]
    either settings_onNotifyException pure result

buildEvent :: Exception.Exception e => BeforeNotify -> e -> Event
buildEvent bn e = runBeforeNotify bn e
    $ defaultEvent { event_exceptions = [ex] }
    where ex = bugsnagExceptionFromSomeException $ Exception.toException e

globalBeforeNotify :: Settings -> BeforeNotify
globalBeforeNotify Settings {..} =
    filterExceptions (not . ignoreException)
        <> settings_beforeNotify
        <> maybe mempty setStackFramesCode settings_codeIndex
        <> updateEvent setApp
  where
    ignoreException e
        | settings_releaseStage `notElem` settings_enabledReleaseStages = True
        | otherwise = settings_ignoreException e

    setApp event = event
        { event_app = Just $ defaultApp
            { app_version = settings_appVersion
            , app_releaseStage = Just settings_releaseStage
            }
        }
