{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Bugsnag.Severity
    ( BugsnagSeverity(..)
    , BugsnagSeverityReason(..)
    , BugsnagSeverityReasonAttributes(..)
    , bugsnagSeverityReasonAttributes
    , BugsnagSeverityReasonType(..)
    ) where

import Data.Aeson
import Data.Aeson.Ext
import Data.Text (Text)
import GHC.Generics

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
    toJSON UnhandledExceptionReasonType = String "unhandledException"
    toJSON UnhandledErrorReasonType = String "unhandledError"
    toJSON LogReasonType = String "log"
    toJSON SignalReasonType = String "signal"
    toJSON StrictModeReasonType = String "strictMode"
    toJSON UnhandledPromiseRejectionReasonType = String "unhandledPromiseRejection"
    toJSON CallbackErrorInterceptReasonType = String "callbackErrorIntercept"
    toJSON ErrorClassReasonType = String "errorClass"
    toJSON UnhandledPanicReasonType = String "unhandledPanic"
    toJSON UserCallbackSetSeverityReasonType = String "userCallbackSetSeverity"
    toJSON UserSpecifiedSeverityReasonType = String "userSpecifiedSeverity"
    toJSON HandledExceptionReasonType = String "handledException"
    toJSON HandledErrorReasonType = String "handledError"
    toJSON HandledPanicReasonType = String "handledPanic"
    toJSON UserContextSetSeverityReasonType = String "userContextSetSeverity"

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

bugsnagSeverityReasonAttributes :: BugsnagSeverityReasonAttributes
bugsnagSeverityReasonAttributes = BugsnagSeverityReasonAttributes
    { bsraErrorType = Nothing
    , bsraLevel = Nothing
    , bsraSignalType = Nothing
    , bsraViolationType = Nothing
    , bsraErrorClass = Nothing
    }
