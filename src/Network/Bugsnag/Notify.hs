{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Network.Bugsnag.Notify
    ( notifyBugsnag
    , notifyBugsnagWith
    ) where

import Control.Exception (SomeException)
import Control.Monad (when)
import Network.Bugsnag.App
import Network.Bugsnag.BeforeNotify
import Network.Bugsnag.Event
import Network.Bugsnag.Exception
import Network.Bugsnag.Report
import Network.Bugsnag.Reporter
import Network.Bugsnag.Settings

-- | Notify Bugsnag of a single exception
notifyBugsnag :: BugsnagSettings -> SomeException -> IO ()
notifyBugsnag = notifyBugsnagWith id

-- | Notify Bugsnag of a single exception, modifying the event
--
-- This is used to (e.g.) change severity for a specific error. Note that the
-- given function runs after any configured @'bsBeforeNotify'@, or changes
-- caused by other aspects of settings (e.g. grouping hash).
--
notifyBugsnagWith :: BeforeNotify -> BugsnagSettings -> SomeException -> IO ()
notifyBugsnagWith f settings ex = do
    let event =
            f
                . bsBeforeNotify settings
                . setGroupingHashBy (bsGroupingHash settings)
                . setStackFramesInProject (bsIsInProject settings)
                . filterStackFrames (bsFilterStackFrames settings)
                . createApp settings
                . bugsnagEvent
                $ bugsnagExceptionFromSomeException ex

        manager = bsHttpManager settings
        apiKey = bsApiKey settings
        report = bugsnagReport [event]

    -- N.B. all notify functions should go through here. We need to maintain
    -- this as the single point where (e.g.) should-notify is checked,
    -- before-notify is applied, stack-frame filtering, etc.
    when (bugsnagShouldNotify settings event)
        $ reportError manager apiKey report

-- |
--
-- N.B. safe to clobber because we're only used on a fresh event.
--
createApp :: BugsnagSettings -> BeforeNotify
createApp settings event = event
    { beApp = Just $ bugsnagApp
        { baVersion = bsAppVersion settings
        , baReleaseStage = Just $ bsReleaseStage settings
        }
    }
