module Network.Bugsnag.Severity
    ( BugsnagSeverity(..)
    , BugsnagSeverityReason(..)
    , BugsnagSeverityReasonAttributes(..)
    , bugsnagSeverityReasonAttributes
    , BugsnagSeverityReasonType(..)
    ) where

import Prelude

import Data.Aeson
import Data.Aeson.Ext
import Data.Text (Text)
import GHC.Generics

data BugsnagSeverity
    = ErrorSeverity
    | WarningSeverity
    | InfoSeverity
    deriving stock Generic

instance ToJSON BugsnagSeverity where
    toJSON = genericToJSON $ bsAesonOptions "Severity"
    toEncoding = genericToEncoding $ bsAesonOptions "Severity"

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
    deriving stock Generic

instance ToJSON BugsnagSeverityReasonType where
    toJSON = genericToJSON $ bsAesonOptions "ReasonType"
    toEncoding = genericToEncoding $ bsAesonOptions "ReasonType"

data BugsnagSeverityReasonAttributes = BugsnagSeverityReasonAttributes
    { bsraErrorType :: Maybe Text
    , bsraLevel :: Maybe Text
    , bsraSignalType :: Maybe Text
    , bsraViolationType :: Maybe Text
    , bsraErrorClass :: Maybe Text
    }
    deriving stock Generic

instance ToJSON BugsnagSeverityReasonAttributes where
    toJSON = genericToJSON $ bsAesonOptions "bsra"
    toEncoding = genericToEncoding $ bsAesonOptions "bsra"

data BugsnagSeverityReason = BugsnagSeverityReason
    { bsrType :: BugsnagSeverityReasonType
    , bsrAttributes :: BugsnagSeverityReasonAttributes
    }
    deriving stock Generic

instance ToJSON BugsnagSeverityReason where
    toJSON = genericToJSON $ bsAesonOptions "bsr"
    toEncoding = genericToEncoding $ bsAesonOptions "bsr"

bugsnagSeverityReasonAttributes :: BugsnagSeverityReasonAttributes
bugsnagSeverityReasonAttributes = BugsnagSeverityReasonAttributes
    { bsraErrorType = Nothing
    , bsraLevel = Nothing
    , bsraSignalType = Nothing
    , bsraViolationType = Nothing
    , bsraErrorClass = Nothing
    }
