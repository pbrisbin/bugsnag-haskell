{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Bugsnag.Event
    ( BugsnagEvent(..)
    , bugsnagEvent

    -- * Update helpers, useful as before-notify arguments
    , updateEventFromRequest
    , updateEventFromSession
    , errorSeverity
    , warningSeverity
    , infoSeverity
    ) where

import Data.Aeson
import Data.Aeson.Ext
import Data.Text (Text)
import GHC.Generics
import Network.Bugsnag.App
import Network.Bugsnag.Breadcrumb
import Network.Bugsnag.Device
import Network.Bugsnag.Exception
import Network.Bugsnag.Request
import Network.Bugsnag.Session
import Network.Bugsnag.Severity
import Network.Bugsnag.Thread
import Network.Bugsnag.User

data BugsnagEvent = BugsnagEvent
    { beExceptions :: [BugsnagException]
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

bugsnagEvent :: [BugsnagException] -> BugsnagEvent
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

updateEventFromRequest :: Applicative m => BugsnagRequest -> BugsnagEvent -> m BugsnagEvent
updateEventFromRequest request event = pure $ event { beRequest = Just request }

updateEventFromSession :: Applicative m => BugsnagSession -> BugsnagEvent -> m BugsnagEvent
updateEventFromSession session event = pure $ event
    { beContext = bsContext session
    , beUser = bsUser session
    }

errorSeverity :: Applicative m => BugsnagEvent -> m BugsnagEvent
errorSeverity = setSeverity ErrorSeverity

warningSeverity :: Applicative m => BugsnagEvent -> m BugsnagEvent
warningSeverity = setSeverity WarningSeverity

infoSeverity :: Applicative m => BugsnagEvent -> m BugsnagEvent
infoSeverity = setSeverity InfoSeverity

setSeverity :: Applicative m => BugsnagSeverity -> BugsnagEvent -> m BugsnagEvent
setSeverity severity event = pure $ event { beSeverity = Just severity }
