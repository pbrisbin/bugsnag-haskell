module Network.Bugsnag.Notify
    ( notifyBugsnag
    , notifyBugsnagWith
    ) where

import Control.Monad (when)
import Network.Bugsnag.App
import Network.Bugsnag.Event
import Network.Bugsnag.Exception
import Network.Bugsnag.Report
import Network.Bugsnag.Reporter
import Network.Bugsnag.Settings
import Network.Bugsnag.StackFrame

-- | Notify Bugsnag of a single exception
notifyBugsnag :: BugsnagSettings -> BugsnagException -> IO ()
notifyBugsnag = notifyBugsnagWith id

-- | Notify Bugsnag of a single exception, modifying the event
--
-- This is used to (e.g.) change severity for a specific error. Note that the
-- given function runs after any configured @'bsBeforeNotify'@, or changes
-- caused by other aspects of setting (e.g. grouping hash).
--
notifyBugsnagWith
    :: (BugsnagEvent -> BugsnagEvent)
    -> BugsnagSettings
    -> BugsnagException
    -> IO ()
notifyBugsnagWith f settings exception =
    -- N.B. all notify functions should go through here. We need to maintain
    -- this as the single point where (e.g.) should-notify is checked,
    -- before-notify is applied, stack-frame filtering, etc.
    when (bugsnagShouldNotify settings) $ do
        let event
                = f
                . bsBeforeNotify settings
                . updateGroupingHash settings
                . updateStackFramesInProject settings
                . filterStackFrames settings
                . createApp settings
                . bugsnagEvent
                $ pure exception

            manager = bsHttpManager settings
            apiKey = bsApiKey settings
            report = bugsnagReport [event]

        reportError manager apiKey report

updateGroupingHash :: BugsnagSettings -> BugsnagEvent -> BugsnagEvent
updateGroupingHash settings event = event
    { beGroupingHash = bsGroupingHash settings event
    }

updateStackFramesInProject :: BugsnagSettings -> BugsnagEvent -> BugsnagEvent
updateStackFramesInProject settings event = event
    { beExceptions = updateExceptions <$> beExceptions event
    }
  where
    updateExceptions :: BugsnagException -> BugsnagException
    updateExceptions ex = ex
        { beStacktrace = map updateStackFrames $ beStacktrace ex
        }

    updateStackFrames :: BugsnagStackFrame -> BugsnagStackFrame
    updateStackFrames sf = sf
        { bsfInProject = Just $ bsIsInProject settings $ bsfFile sf
        }

filterStackFrames :: BugsnagSettings -> BugsnagEvent -> BugsnagEvent
filterStackFrames settings event = event
    { beExceptions = updateExceptions <$> beExceptions event
    }
  where
    updateExceptions :: BugsnagException -> BugsnagException
    updateExceptions ex = ex
        { beStacktrace = filter (bsFilterStackFrames settings) $ beStacktrace ex
        }

-- |
--
-- N.B. safe to clobber because we're only used on a fresh event.
--
createApp :: BugsnagSettings -> BugsnagEvent -> BugsnagEvent
createApp settings event = event
    { beApp = Just $ bugsnagApp
        { baVersion = bsAppVersion settings
        , baReleaseStage = Just $ bsReleaseStage settings
        }
    }
