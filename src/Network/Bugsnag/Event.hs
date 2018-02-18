{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Bugsnag.Event
    ( BugsnagEvent(..)
    , bugsnagEvent

    -- * Update helpers, useful as before-notify arguments
    , updateEventFromRequest
    , updateEventFromSession
    , setStacktrace
    , errorSeverity
    , warningSeverity
    , infoSeverity
    ) where

import Data.Aeson
import Data.Aeson.Ext
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import GHC.Generics
import Network.Bugsnag.App
import Network.Bugsnag.Breadcrumb
import Network.Bugsnag.Device
import Network.Bugsnag.Exception
import Network.Bugsnag.Request
import Network.Bugsnag.Session
import Network.Bugsnag.Severity
import Network.Bugsnag.StackFrame
import Network.Bugsnag.Thread
import Network.Bugsnag.User

data BugsnagEvent = BugsnagEvent
    { beExceptions :: NonEmpty BugsnagException
    , beBreadcrumbs :: Maybe [BugsnagBreadcrumb]
    , beRequest :: Maybe BugsnagRequest
    , beThreads :: Maybe [BugsnagThread]
    , beContext :: Maybe Text
    , beGroupingHash :: Maybe Text
    , beUnhandled :: Maybe Bool
    , beSeverity :: Maybe BugsnagSeverity
    , beSeverityReason :: Maybe BugsnagSeverityReason
    , beUser :: Maybe BugsnagUser
    , beApp :: Maybe BugsnagApp
    , beDevice :: Maybe BugsnagDevice
    --, beSession
    -- N.B. omitted because it's an object specific to the Session Tracking API,
    -- and I'm not sure yet how to resolve the naming clash with BugsnagSession.
    , beMetaData :: Maybe Value
    }
    deriving Generic

instance ToJSON BugsnagEvent where
    toJSON = genericToJSON $ lowerDroppingPrefix "be"
    toEncoding = genericToEncoding $ lowerDroppingPrefix "be"

bugsnagEvent :: NonEmpty BugsnagException -> BugsnagEvent
bugsnagEvent exceptions = BugsnagEvent
    { beExceptions = exceptions
    , beBreadcrumbs = Nothing
    , beRequest = Nothing
    , beThreads = Nothing
    , beContext = Nothing
    , beGroupingHash = Nothing
    , beUnhandled = Nothing
    , beSeverity = Nothing
    , beSeverityReason = Nothing
    , beUser = Nothing
    , beApp = Nothing
    , beDevice = Nothing
    , beMetaData = Nothing
    }

-- | Set the Event's Request
--
-- See also: @'bugsnagRequestFromWaiRequest'@
--
updateEventFromRequest :: BugsnagRequest -> BugsnagEvent -> BugsnagEvent
updateEventFromRequest request event = event { beRequest = Just request }

-- | Update the Event's Context and User from the Session
updateEventFromSession :: BugsnagSession -> BugsnagEvent -> BugsnagEvent
updateEventFromSession session event = event
    { beContext = bsContext session
    , beUser = bsUser session
    }

-- | Set the stacktrace on the reported exception
--
-- > notifyBugsnagWith (setStacktrace [$(currentStackFrame) "myFunc"]) ...
--
setStacktrace :: [BugsnagStackFrame] -> BugsnagEvent -> BugsnagEvent
setStacktrace stacktrace event = event
    { beExceptions = (\ex -> ex { beStacktrace = stacktrace })
        <$> beExceptions event
    }

-- | Set to @'ErrorSeverity'@
errorSeverity :: BugsnagEvent -> BugsnagEvent
errorSeverity = setSeverity ErrorSeverity

-- | Set to @'WarningSeverity'@
warningSeverity :: BugsnagEvent -> BugsnagEvent
warningSeverity = setSeverity WarningSeverity

-- | Set to @'InfoSeverity'@
infoSeverity :: BugsnagEvent -> BugsnagEvent
infoSeverity = setSeverity InfoSeverity

setSeverity :: BugsnagSeverity -> BugsnagEvent -> BugsnagEvent
setSeverity severity event = event { beSeverity = Just severity }
