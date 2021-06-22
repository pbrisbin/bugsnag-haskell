module Network.Bugsnag.Breadcrumb
    ( BugsnagBreadcrumbType(..)
    , BugsnagBreadcrumb(..)
    , bugsnagBreadcrumb
    ) where

import Prelude

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
    deriving stock Generic

instance ToJSON BugsnagBreadcrumbType where
    toJSON = genericToJSON $ bsAesonOptions "Breadcrumb"
    toEncoding = genericToEncoding $ bsAesonOptions "Breadcrumb"

data BugsnagBreadcrumb = BugsnagBreadcrumb
    { bbTimestamp :: UTCTime
    , bbName :: Text
    , bbType :: BugsnagBreadcrumbType
    , bbMetaData :: Maybe Value
    }
    deriving stock Generic

instance ToJSON BugsnagBreadcrumb where
    toJSON = genericToJSON $ bsAesonOptions "bb"
    toEncoding = genericToEncoding $ bsAesonOptions "bb"

bugsnagBreadcrumb
    :: UTCTime -> Text -> BugsnagBreadcrumbType -> BugsnagBreadcrumb
bugsnagBreadcrumb timestamp name typ = BugsnagBreadcrumb
    { bbTimestamp = timestamp
    , bbName = name
    , bbType = typ
    , bbMetaData = Nothing
    }
