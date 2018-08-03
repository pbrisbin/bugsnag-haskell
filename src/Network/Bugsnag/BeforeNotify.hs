{-# LANGUAGE OverloadedStrings #-}

module Network.Bugsnag.BeforeNotify
    ( BeforeNotify
    , defaultBeforeNotify

    -- * Modifying the Exception
    , updateException

    -- * Modifying the Event
    , updateEventFromSession
    , updateEventFromWaiRequest

    -- * Modifying the Request
    , redactRequestHeaders

    -- * Simple setters
    , setDevice
    , setRequest
    , setStacktrace

    -- * Setting severity
    , setWarningSeverity
    , setErrorSeverity
    , setInfoSeverity
    ) where

import Network.Bugsnag.Device
import Network.Bugsnag.Event
import Network.Bugsnag.Exception
import Network.Bugsnag.Request
import Network.Bugsnag.Session
import Network.Bugsnag.Severity
import Network.Bugsnag.StackFrame
import Network.HTTP.Types.Header (Header, HeaderName)
import Network.Wai (Request)

type BeforeNotify = BugsnagEvent -> BugsnagEvent

-- | Used as @'bsBeforeNotify'@ the default Settings value
--
-- Redacts the following Request headers:
--
-- - Authorization
-- - Cookie
-- - X-XSRF-TOKEN (CSRF token header used by Yesod)
--
-- N.B. If you override the value on @'BugsnagSettings'@, you probably want to
-- maintain this as well:
--
-- @
-- settings { 'bsBeforeNotify' = 'defaultBeforeNotify' . myBeforeNotify }
-- @
--
defaultBeforeNotify :: BeforeNotify
defaultBeforeNotify =
    redactRequestHeaders ["Authorization", "Cookie", "X-XSRF-TOKEN"]

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
-- > forSqlError ex =
-- >     case fromException =<< beOriginalException ex of
-- >         Just SqlError{..} -> ex
-- >             { beErrorClass = "SqlError-" <> sqlErrorCode
-- >             , beMessage = Just sqlErrorMessage
-- >             }
-- >         _ -> ex
--
updateException :: (BugsnagException -> BugsnagException) -> BeforeNotify
updateException f event = event { beExceptions = f <$> beExceptions event }

-- | Set the events @'BugsnagEvent'@ and @'BugsnagDevice'@
updateEventFromWaiRequest :: Request -> BeforeNotify
updateEventFromWaiRequest wrequest =
    let
        mdevice = bugsnagDeviceFromWaiRequest wrequest
        request = bugsnagRequestFromWaiRequest wrequest
    in maybe id setDevice mdevice . setRequest request

-- | Update the Event's Context and User from the Session
updateEventFromSession :: BugsnagSession -> BeforeNotify
updateEventFromSession session event =
    event { beContext = bsContext session, beUser = bsUser session }

-- | Redact the given request headers
--
-- Headers like @Authorization@ may contain information you don't want to report
-- to Bugsnag.
--
-- > redactRequestHeaders ["Authorization", "Cookie"]
--
redactRequestHeaders :: [HeaderName] -> BeforeNotify
redactRequestHeaders headers event =
    event { beRequest = redactHeaders headers <$> beRequest event }

-- |
--
-- >>> let headers = [("Authorization", "secret"), ("X-Foo", "Bar")]
-- >>> let req = bugsnagRequest { brHeaders = Just headers }
-- >>> brHeaders $ redactHeaders ["Authorization"] req
-- Just [("Authorization","<redacted>"),("X-Foo","Bar")]
--
redactHeaders :: [HeaderName] -> BugsnagRequest -> BugsnagRequest
redactHeaders headers request = request
    { brHeaders = map redactHeader <$> brHeaders request
    }
  where
    redactHeader :: Header -> Header
    redactHeader (k, _) | k `elem` headers = (k, "<redacted>")
    redactHeader h = h

-- | Set the Event's Request
--
-- See @'bugsnagRequestFromWaiRequest'@
--
setRequest :: BugsnagRequest -> BeforeNotify
setRequest request event = event { beRequest = Just request }

-- | Set the Event's Device
--
-- See @'bugsnagDeviceFromWaiRequest'@
--
setDevice :: BugsnagDevice -> BeforeNotify
setDevice device event = event { beDevice = Just device }

-- | Set the stacktrace on the reported exception
--
-- > notifyBugsnagWith (setStacktrace [$(currentStackFrame) "myFunc"]) ...
--
setStacktrace :: [BugsnagStackFrame] -> BeforeNotify
setStacktrace stacktrace =
    updateException $ \ex -> ex { beStacktrace = stacktrace }

-- | Set to @'ErrorSeverity'@
setErrorSeverity :: BeforeNotify
setErrorSeverity = setSeverity ErrorSeverity

-- | Set to @'WarningSeverity'@
setWarningSeverity :: BeforeNotify
setWarningSeverity = setSeverity WarningSeverity

-- | Set to @'InfoSeverity'@
setInfoSeverity :: BeforeNotify
setInfoSeverity = setSeverity InfoSeverity

setSeverity :: BugsnagSeverity -> BeforeNotify
setSeverity severity event = event { beSeverity = Just severity }
