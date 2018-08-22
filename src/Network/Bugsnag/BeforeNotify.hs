{-# LANGUAGE OverloadedStrings #-}

module Network.Bugsnag.BeforeNotify
    ( BeforeNotify
    , defaultBeforeNotify

    -- * Modifying the Exception
    , updateException
    , filterStackFrames
    , setStackFramesInProject
    , setGroupingHash
    , setGroupingHashBy

    -- * Modifying the Event
    , updateEventFromException
    , updateEventFromOriginalException
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

import Control.Exception (Exception, fromException)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
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
updateException f event = event { beException = f $ beException event }

-- | Filter out StackFrames matching a predicate
filterStackFrames :: (BugsnagStackFrame -> Bool) -> BeforeNotify
filterStackFrames p =
    updateException $ \ex -> ex { beStacktrace = filter p $ beStacktrace ex }

-- | Set @'bsIsInProject'@ using the given predicate, applied to the Filename
setStackFramesInProject :: (FilePath -> Bool) -> BeforeNotify
setStackFramesInProject p = updateException
    $ \ex -> ex { beStacktrace = map updateStackFrames $ beStacktrace ex }
  where
    updateStackFrames :: BugsnagStackFrame -> BugsnagStackFrame
    updateStackFrames sf = sf { bsfInProject = Just $ p $ bsfFile sf }

-- | Set @'beGroupingHash'@
setGroupingHash :: Text -> BeforeNotify
setGroupingHash hash = setGroupingHashBy $ const $ Just hash

-- | Set @'beGroupingHash'@ based on the Event
setGroupingHashBy :: (BugsnagEvent -> Maybe Text) -> BeforeNotify
setGroupingHashBy f event = event { beGroupingHash = f event }

-- | Update the @'BugsnagEvent'@ based on its @'BugsnagException'@
--
-- Use this instead of @'updateException'@ if you want to do other things to the
-- Event, such as set its @'beGroupingHash'@ based on the Exception.
--
updateEventFromException :: (BugsnagException -> BeforeNotify) -> BeforeNotify
updateEventFromException f event = f (beException event) event

-- | Update the @'BugsnagEvent'@ based on the original exception
--
-- This allows updating the Event after casting to an exception type that this
-- library doesn't know about (e.g. @SqlError@). Because the result of your
-- function is itself a @'BeforeNotify'@, you can (and should) use other
-- helpers:
--
-- @
-- myBeforeNotify =
--     'defaultBeforeNotify'
--         . 'updateEventFromOriginalException' asSqlError
--         . 'updateEventFromOriginalException' asHttpError
--         . -- ...
--
-- asSqlError :: SqlError -> BeforeNotify
-- asSqlError SqlError{..} =
--     'setGroupingHash' sqlErrorCode . 'updateException' $ \ex -> ex
--         { beErrorClass = sqlErrorCode
--         , beMessage = sqlErrorMessage
--         }
-- @
--
-- If there is no original exception, or the cast fails, the event is unchanged.
--
updateEventFromOriginalException
    :: Exception e => (e -> BeforeNotify) -> BeforeNotify
updateEventFromOriginalException f event = fromMaybe event $ do
    someException <- beOriginalException $ beException event
    yourException <- fromException someException
    pure $ f yourException event

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
