module Network.Bugsnag.BeforeNotify
    ( BeforeNotify
    , setStacktrace
    , updateEventFromRequest
    , updateEventFromSession
    , updateEventFromWaiRequest

    -- * Setting severity
    , warningSeverity
    , errorSeverity
    , infoSeverity
    ) where

import Network.Bugsnag.Device
import Network.Bugsnag.Event
import Network.Bugsnag.Exception
import Network.Bugsnag.Request
import Network.Bugsnag.Session
import Network.Bugsnag.Severity
import Network.Bugsnag.StackFrame
import Network.Wai (Request)

type BeforeNotify = BugsnagEvent -> BugsnagEvent

-- | Set the events @'BugsnagEvent'@ and @'BugsnagDevice'@
updateEventFromWaiRequest :: Request -> BeforeNotify
updateEventFromWaiRequest wrequest =
    let mdevice = bugsnagDeviceFromWaiRequest wrequest
        request = bugsnagRequestFromWaiRequest wrequest
    in maybe id setDevice mdevice . updateEventFromRequest request

-- | Set the Event's Request
--
-- See also: @'bugsnagRequestFromWaiRequest'@
--
updateEventFromRequest :: BugsnagRequest -> BeforeNotify
updateEventFromRequest request event = event { beRequest = Just request }

-- | Update the Event's Context and User from the Session
updateEventFromSession :: BugsnagSession -> BeforeNotify
updateEventFromSession session event = event
    { beContext = bsContext session
    , beUser = bsUser session
    }

-- | Set the device on the event
--
-- See @'bugsnagDeviceFromUserAgent'@
--
setDevice :: BugsnagDevice -> BeforeNotify
setDevice device event = event
    { beDevice = Just device
    }

-- | Set the stacktrace on the reported exception
--
-- > notifyBugsnagWith (setStacktrace [$(currentStackFrame) "myFunc"]) ...
--
setStacktrace :: [BugsnagStackFrame] -> BeforeNotify
setStacktrace stacktrace event = event
    { beExceptions = (\ex -> ex { beStacktrace = stacktrace })
        <$> beExceptions event
    }

-- | Set to @'ErrorSeverity'@
errorSeverity :: BeforeNotify
errorSeverity = setSeverity ErrorSeverity

-- | Set to @'WarningSeverity'@
warningSeverity :: BeforeNotify
warningSeverity = setSeverity WarningSeverity

-- | Set to @'InfoSeverity'@
infoSeverity :: BeforeNotify
infoSeverity = setSeverity InfoSeverity

setSeverity :: BugsnagSeverity -> BeforeNotify
setSeverity severity event = event { beSeverity = Just severity }
