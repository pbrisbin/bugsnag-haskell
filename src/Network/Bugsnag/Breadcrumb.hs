{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Bugsnag.Breadcrumb
    ( BugsnagBreadcrumbType(..)
    , BugsnagBreadcrumb(..)
    , bugsnagBreadcrumb
    ) where

import Data.Aeson
import Data.Aeson.Ext
import Data.Text (Text)
import Data.Time
import GHC.Generics

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

bugsnagBreadcrumb
    :: UTCTime
    -> Text
    -> BugsnagBreadcrumbType
    -> BugsnagBreadcrumb
bugsnagBreadcrumb timestamp name typ = BugsnagBreadcrumb
    { bbTimestamp = timestamp
    , bbName = name
    , bbType = typ
    , bbMetaData = Nothing
    }
