module Network.Bugsnag.Event
    ( BugsnagEvent(..)
    , bugsnagEvent
    ) where

import Prelude

import Data.Aeson
import Data.Aeson.Ext
import Data.Aeson.Types
import Data.Text (Text)
import Network.Bugsnag.App
import Network.Bugsnag.Breadcrumb
import Network.Bugsnag.Device
import Network.Bugsnag.Exception
import Network.Bugsnag.Request
import Network.Bugsnag.Severity
import Network.Bugsnag.Thread
import Network.Bugsnag.User

data BugsnagEvent = BugsnagEvent
    { beException :: BugsnagException
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

instance ToJSON BugsnagEvent where
    -- | Explicit instance needed to send @'beException'@ as @exceptions@
    toJSON BugsnagEvent {..} = object $ "exceptions" .= [beException] : concat
        [ "breadcrumbs" .=? beBreadcrumbs
        , "request" .=? beRequest
        , "threads" .=? beThreads
        , "context" .=? beContext
        , "groupingHash" .=? beGroupingHash
        , "unhandled" .=? beUnhandled
        , "severity" .=? beSeverity
        , "severityReason" .=? beSeverityReason
        , "user" .=? beUser
        , "app" .=? beApp
        , "device" .=? beDevice
        , "metaData" .=? beMetaData
        ]
      where
        -- For implementing "omit Nothing fields"
        (.=?) :: ToJSON v => Text -> Maybe v -> [Pair]
        (.=?) k = maybe [] (pure . (fromText k .=))

bugsnagEvent :: BugsnagException -> BugsnagEvent
bugsnagEvent exception = BugsnagEvent
    { beException = exception
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
