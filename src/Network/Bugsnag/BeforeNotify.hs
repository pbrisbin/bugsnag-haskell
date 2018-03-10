module Network.Bugsnag.BeforeNotify
    ( BeforeNotify
    , setStacktrace
    , updateException
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

-- | Modify just the Exception part of an Event
--
-- Technically this will modify all exceptions in the Event, but if you're using
-- this library normally, there will be only one.
--
-- This may be used to set more specific information for exception types in
-- scope in your application:
--
-- > notifyBugsnagWith (updateException forSqlError) settings ex
-- >
-- > forSqlError :: BugsnagException -> BugsnagException
-- > forSqlError ex = case fromException @SqlError ex of
-- >    Just SqlError{..} -> ex
-- >      { beErrorClass = "SqlError-" <> sqlErrorCode
-- >      , beMessage = sqlErrorMessage
-- >      }
--
updateException :: (BugsnagException -> BugsnagException) -> BeforeNotify
updateException f event = event
    { beExceptions = f <$> beExceptions event
    }

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
setStacktrace stacktrace = updateException $ \ex -> ex
    { beStacktrace = stacktrace
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
