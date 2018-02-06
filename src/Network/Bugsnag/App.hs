{-# LANGUAGE DeriveGeneric #-}
module Network.Bugsnag.App
    ( BugsnagApp(..)
    , bugsnagApp
    , bugsnagAppWithVersion
    ) where

import Data.Aeson
import Data.Aeson.Ext
import Data.Text (Text)
import Data.Version
import GHC.Generics
import Network.Bugsnag.ReleaseStage
import Numeric.Natural

data BugsnagApp = BugsnagApp
    { baId :: Maybe Text
    , baVersion :: Maybe Version
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

bugsnagAppWithVersion :: Version -> BugsnagApp
bugsnagAppWithVersion version = bugsnagApp
    { baVersion = Just version
    }
