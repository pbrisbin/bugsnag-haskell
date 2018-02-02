{-# LANGUAGE DeriveGeneric #-}
module Network.Bugsnag.User
    ( BugsnagUser(..)
    , bugsnagUser
    ) where

import Data.Aeson
import Data.Aeson.Ext
import Data.Text (Text)
import GHC.Generics

data BugsnagUser = BugsnagUser
    { buId :: Maybe Text
    , buEmailAddress :: Maybe Text
    , buUsername :: Maybe Text
    }
    deriving Generic

instance ToJSON BugsnagUser where
    toJSON = genericToJSON $ lowerDroppingPrefix "bu"
    toEncoding = genericToEncoding $ lowerDroppingPrefix "bu"

bugsnagUser :: BugsnagUser
bugsnagUser = BugsnagUser
    { buId = Nothing
    , buEmailAddress = Nothing
    , buUsername = Nothing
    }
