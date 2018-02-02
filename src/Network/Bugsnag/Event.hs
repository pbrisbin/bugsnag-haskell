{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
--
-- <https://bugsnagerrorreportingapi.docs.apiary.io/#reference/0/notify/send-error-reports>
--
module Network.Bugsnag.Event
    ( BugsnagRequest(..)
    , BugsnagThread(..)
    , BugsnagApp(..)
    , BugsnagDevice(..)

    -- * Breadcrumbs
    , BugsnagBreadcrumbType(..)
    , BugsnagBreadcrumb(..)

    -- * Severity
    , BugsnagSeverity(..)
    , BugsnagSeverityReasonType(..)
    , BugsnagSeverityReasonAttributes(..)
    , BugsnagSeverityReason(..)

    -- * Overall event
    , BugsnagEvent(..)
    , bugsnagEvent
    , updateEventFromSession
    ) where

import Data.Aeson
import Data.Aeson.Ext
import Data.Text (Text)
import Data.Time
import GHC.Generics
import Network.Bugsnag.Exception
import Network.Bugsnag.Session
import Network.Bugsnag.Settings
import Network.Bugsnag.User
import Numeric.Natural

data BugsnagBreadcrumbType
    = NavigationBreadcrumb
    | RequestBreadcrumb
    | ProcessBreadcrumb
    | LogBreadcrumb
    | UserBreadcrumb
    | StateBreadcrumb
    | ErrorBreadcrumb
    | ManualBreadcrumb

instance ToJSON BugsnagBreadcrumbType where
    toJSON NavigationBreadcrumb = String "navigation"
    toJSON RequestBreadcrumb = String "request"
    toJSON ProcessBreadcrumb = String "process"
    toJSON LogBreadcrumb = String "log"
    toJSON UserBreadcrumb = String "user"
    toJSON StateBreadcrumb = String "state"
    toJSON ErrorBreadcrumb = String "error"
    toJSON ManualBreadcrumb = String "manual"

data BugsnagBreadcrumb = BugsnagBreadcrumb
    { bbTimestamp :: UTCTime
    , bbName :: Text
    , bbType :: BugsnagBreadcrumbType
    , bbMetaData :: Maybe Value
    }
    deriving Generic

instance ToJSON BugsnagBreadcrumb where
    toJSON = genericToJSON $ lowerDroppingPrefix "bb"
    toEncoding = genericToEncoding $ lowerDroppingPrefix "bb"

-- | The web request being handled when the error was encountered
data BugsnagRequest = BugsnagRequest
    { brClientIp :: Maybe Text
    , brHeaders :: Maybe [(Text, Text)] -- FIXME: [Header]
    , brHttpMethod :: Maybe Text -- FIXME: Method
    , brUrl :: Maybe Text -- FIXME: URI
    , brReferer :: Maybe Text
    }
    deriving Generic

instance ToJSON BugsnagRequest where
    toJSON = genericToJSON $ lowerDroppingPrefix "br"
    toEncoding = genericToEncoding $ lowerDroppingPrefix "br"

data BugsnagThread = BugsnagThread
    { btId :: Maybe Text
    , btName :: Maybe Text
    , btStacktrace :: Maybe [BugsnagStackFrame]
    }
    deriving Generic

instance ToJSON BugsnagThread where
    toJSON = genericToJSON $ lowerDroppingPrefix "bt"
    toEncoding = genericToEncoding $ lowerDroppingPrefix "bt"

data BugsnagSeverity
    = ErrorSeverity
    | WarningSeverity
    | InfoSeverity

instance ToJSON BugsnagSeverity where
    toJSON ErrorSeverity = String "error"
    toJSON WarningSeverity = String "warning"
    toJSON InfoSeverity = String "info"

data BugsnagSeverityReasonType
    = UnhandledExceptionReasonType
    | UnhandledErrorReasonType
    | LogReasonType
    | SignalReasonType
    | StrictModeReasonType
    | UnhandledPromiseRejectionReasonType
    | CallbackErrorInterceptReasonType
    | ErrorClassReasonType
    | UnhandledPanicReasonType
    | UserCallbackSetSeverityReasonType
    | UserSpecifiedSeverityReasonType
    | HandledExceptionReasonType
    | HandledErrorReasonType
    | HandledPanicReasonType
    | UserContextSetSeverityReasonType

instance ToJSON BugsnagSeverityReasonType where
    toJSON UnhandledExceptionReasonType = String "unhandledExceptionReasonType"
    toJSON UnhandledErrorReasonType = String "unhandledErrorReasonType"
    toJSON LogReasonType = String "logReasonType"
    toJSON SignalReasonType = String "signalReasonType"
    toJSON StrictModeReasonType = String "strictModeReasonType"
    toJSON UnhandledPromiseRejectionReasonType = String "unhandledPromiseRejectionReasonType"
    toJSON CallbackErrorInterceptReasonType = String "callbackErrorInterceptReasonType"
    toJSON ErrorClassReasonType = String "errorClassReasonType"
    toJSON UnhandledPanicReasonType = String "unhandledPanicReasonType"
    toJSON UserCallbackSetSeverityReasonType = String "userCallbackSetSeverityReasonType"
    toJSON UserSpecifiedSeverityReasonType = String "userSpecifiedSeverityReasonType"
    toJSON HandledExceptionReasonType = String "handledExceptionReasonType"
    toJSON HandledErrorReasonType = String "handledErrorReasonType"
    toJSON HandledPanicReasonType = String "handledPanicReasonType"
    toJSON UserContextSetSeverityReasonType = String "userContextSetSeverityReasonType"

data BugsnagSeverityReasonAttributes = BugsnagSeverityReasonAttributes
    { bsraErrorType :: Maybe Text
    , bsraLevel :: Maybe Text
    , bsraSignalType :: Maybe Text
    , bsraViolationType :: Maybe Text
    , bsraErrorClass :: Maybe Text
    }
    deriving Generic

instance ToJSON BugsnagSeverityReasonAttributes where
    toJSON = genericToJSON $ lowerDroppingPrefix "bsra"
    toEncoding = genericToEncoding $ lowerDroppingPrefix "bsra"

data BugsnagSeverityReason = BugsnagSeverityReason
    { bsrType :: BugsnagSeverityReasonType
    , bsrAttributes :: BugsnagSeverityReasonAttributes
    }
    deriving Generic

instance ToJSON BugsnagSeverityReason where
    toJSON = genericToJSON $ lowerDroppingPrefix "bsr"
    toEncoding = genericToEncoding $ lowerDroppingPrefix "bsr"

data BugsnagApp = BugsnagApp
    { baId :: Maybe Text
    , baVersion :: Maybe Text
    , baBuildUUID :: Maybe Text
    , baReleaseStage :: Maybe BugsnagReleaseStage
    , baType :: Maybe Text
    , baDsymUUIDs :: Maybe [Text]
    , baDuration :: Maybe Natural
    , baDurationInForeground :: Maybe Natural
    , baInForeground :: Maybe Bool
    }
    deriving Generic

instance ToJSON BugsnagApp where
    toJSON = genericToJSON $ lowerDroppingPrefix "ba"
    toEncoding = genericToEncoding $ lowerDroppingPrefix "ba"

newtype Bytes = Bytes Natural deriving ToJSON

data BugsnagDevice = BugsnagDevice
    { bdHostname :: Maybe Text
    , bdId :: Maybe Text
    , bdManufacturer :: Maybe Text
    , bdModel :: Maybe Text
    , bdModelNumber :: Maybe Text
    , bdOsName :: Maybe Text
    , bdOsVersion :: Maybe Text
    , bdFreeMemory:: Maybe Bytes
    , bdTotalMemory :: Maybe Bytes
    , bdFreeDisk :: Maybe Bytes
    , bdBrowserName :: Maybe Text
    , bdBrowserVersion :: Maybe Text
    , bdJailBroken :: Maybe Bool
    , bdOrientation :: Maybe Text
    }
    deriving Generic

instance ToJSON BugsnagDevice where
    toJSON = genericToJSON $ lowerDroppingPrefix "bd"
    toEncoding = genericToEncoding $ lowerDroppingPrefix "bd"

-- N.B. session is omitted because it's not the same object as a BugsnagSession
-- and I've not yet decided how to deal with it.
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

updateEventFromSession :: BugsnagSession -> BugsnagEvent -> BugsnagEvent
updateEventFromSession session event = event
    { beContext = bsContext session
    , beUser = bsUser session
    }
