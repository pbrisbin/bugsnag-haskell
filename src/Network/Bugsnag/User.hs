module Network.Bugsnag.User
    ( BugsnagUser(..)
    , bugsnagUser
    ) where

import Prelude

import Data.Aeson
import Data.Aeson.Ext
import Data.Text (Text)
import GHC.Generics

data BugsnagUser = BugsnagUser
    { buId :: Maybe Text
    , buEmailAddress :: Maybe Text
    , buUsername :: Maybe Text
    }
    deriving stock Generic

instance ToJSON BugsnagUser where
    toJSON = genericToJSON $ bsAesonOptions "bu"
    toEncoding = genericToEncoding $ bsAesonOptions "bu"

bugsnagUser :: BugsnagUser
bugsnagUser = BugsnagUser
    { buId = Nothing
    , buEmailAddress = Nothing
    , buUsername = Nothing
    }
