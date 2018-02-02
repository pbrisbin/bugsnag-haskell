{-# LANGUAGE DeriveGeneric #-}
module Network.Bugsnag.App
    ( BugsnagApp(..)
    , bugsnagApp
    , updateAppFromSettings
    ) where

import Data.Aeson
import Data.Aeson.Ext
import Data.Text (Text)
import GHC.Generics
import Network.Bugsnag.Settings
import Numeric.Natural

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

bugsnagApp :: BugsnagApp
bugsnagApp = BugsnagApp
    { baId = Nothing
    , baVersion = Nothing
    , baBuildUUID = Nothing
    , baReleaseStage = Nothing
    , baType = Nothing
    , baDsymUUIDs = Nothing
    , baDuration = Nothing
    , baDurationInForeground = Nothing
    , baInForeground = Nothing
    }

updateAppFromSettings :: BugsnagSettings -> BugsnagApp -> BugsnagApp
updateAppFromSettings settings app = app
    { baReleaseStage = Just $ bsReleaseStage settings
    }
