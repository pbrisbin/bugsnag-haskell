{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- |
--
-- <https://docs.bugsnag.com/api/error-reporting/#per-session-settings>
--
module Network.Bugsnag.Session
    ( BugsnagSession(..)
    , bugsnagSession
    )
where

import Prelude

import Data.Aeson
import Data.Aeson.Ext
import Data.Text (Text)
import GHC.Generics
import Network.Bugsnag.User

data BugsnagSession = BugsnagSession
    { bsUser :: Maybe BugsnagUser
    , bsContext :: Maybe Text
    , bsMetaData :: Maybe Value
    }
    deriving stock Generic

instance ToJSON BugsnagSession where
    toJSON = genericToJSON $ bsAesonOptions "bs"
    toEncoding = genericToEncoding $ bsAesonOptions "bs"

bugsnagSession :: BugsnagSession
bugsnagSession = BugsnagSession
    { bsUser = Nothing
    , bsContext = Nothing
    , bsMetaData = Nothing
    }
