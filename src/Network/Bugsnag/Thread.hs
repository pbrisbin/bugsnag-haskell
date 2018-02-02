{-# LANGUAGE DeriveGeneric #-}
module Network.Bugsnag.Thread
    ( BugsnagThread(..)
    , bugsnagThread
    ) where

import Data.Aeson
import Data.Aeson.Ext
import Data.Text (Text)
import GHC.Generics
import Network.Bugsnag.Exception

data BugsnagThread = BugsnagThread
    { btId :: Maybe Text
    , btName :: Maybe Text
    , btStacktrace :: Maybe [BugsnagStackFrame]
    }
    deriving Generic

instance ToJSON BugsnagThread where
    toJSON = genericToJSON $ lowerDroppingPrefix "bt"
    toEncoding = genericToEncoding $ lowerDroppingPrefix "bt"

bugsnagThread :: BugsnagThread
bugsnagThread = BugsnagThread
    { btId = Nothing
    , btName = Nothing
    , btStacktrace = Nothing
    }
