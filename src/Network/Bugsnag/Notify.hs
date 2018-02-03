module Network.Bugsnag.Notify
    ( notifyBugsnag
    , notifyBugsnagWith
    , notifyBugsnagThrow
    ) where

import Control.Monad (when)
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO, liftIO)
import Network.Bugsnag.Event
import Network.Bugsnag.Exception
import Network.Bugsnag.Report
import Network.Bugsnag.Reporter
import Network.Bugsnag.Settings

-- | Notify Bugsnag of a single exception
notifyBugsnag :: MonadIO m => BugsnagSettings m -> BugsnagException -> m ()
notifyBugsnag = notifyBugsnagWith pure

-- | Notify Bugsnag of a single exception, modifying the event
--
-- This is used to (e.g.) change severity for a specific error. Note that the
-- given function runs after any configured @'bsBeforeNotify'@.
--
notifyBugsnagWith
    :: MonadIO m
    => (BugsnagEvent -> m BugsnagEvent)
    -> BugsnagSettings m
    -> BugsnagException
    -> m ()
notifyBugsnagWith f settings exception =
    when (bugsnagShouldNotify settings) $ do
        event <- f =<< bsBeforeNotify settings (bugsnagEvent [exception])

        let manager = bsHttpManager settings
            apiKey = bsApiKey settings
            report = bugsnagReport [event]

        liftIO $ reportError manager apiKey report

-- | Notify Bugsnag of a single exception and re-throw it
notifyBugsnagThrow
    :: (MonadIO m, MonadThrow m) => BugsnagSettings m -> BugsnagException -> m a
notifyBugsnagThrow settings ex = notifyBugsnag settings ex >> throwM ex
