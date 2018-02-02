module Network.Bugsnag
    ( notifyBugsnag
    , module X
    ) where

import Control.Monad (when)
import Network.Bugsnag.Event as X
import Network.Bugsnag.Exception as X
import Network.Bugsnag.Reporter as X
import Network.Bugsnag.Session as X
import Network.Bugsnag.Settings as X

-- | Notify Bugsnag of a single exception
notifyBugsnag :: BugsnagSettings -> BugsnagSession -> BugsnagException -> IO ()
notifyBugsnag settings session
    = notifyBugsnagEvent settings
    . updateEventFromSession session
    . bugsnagEvent . pure

-- | Notify Bugsnag of a single event
notifyBugsnagEvent :: BugsnagSettings -> BugsnagEvent -> IO ()
notifyBugsnagEvent settings = notifyBugsnagEvents settings . pure

-- | Notify Bugsnag of events
notifyBugsnagEvents :: BugsnagSettings -> [BugsnagEvent] -> IO ()
notifyBugsnagEvents settings events = do
    let manager = bsHttpManager settings
        apiKey = bsApiKey settings
    when (shouldNotify settings) $ reportError manager apiKey events
