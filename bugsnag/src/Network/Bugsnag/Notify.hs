module Network.Bugsnag.Notify
    ( notifyBugsnag
    , notifyBugsnagWith
    ) where

import Prelude

import Control.Exception (SomeException, fromException, toException)
import qualified Control.Exception as Exception
import Control.Exception.Annotated (AnnotatedException)
import qualified Control.Exception.Annotated as Annotated
import Control.Monad (unless, (<=<))
import Data.Annotation (tryAnnotations)
import Data.Bugsnag
import Data.Bugsnag.Settings
import Data.Foldable (fold)
import Data.List.NonEmpty (nonEmpty)
import Network.Bugsnag.BeforeNotify
import Network.Bugsnag.Exception
import Network.Bugsnag.MetaData
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
    $ defaultEvent { event_exceptions = [ex], event_metaData = unMetaData <$> metaDataFromException e }
    where ex = bugsnagExceptionFromSomeException $ Exception.toException e

metaDataFromException :: Exception.Exception e => e -> Maybe MetaData
metaDataFromException = metaDataFromAnnotatedException <=< (fromException @(AnnotatedException SomeException) . toException)

metaDataFromAnnotatedException :: AnnotatedException e -> Maybe MetaData
metaDataFromAnnotatedException = fmap fold . nonEmpty . fst . tryAnnotations . Annotated.annotations

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
