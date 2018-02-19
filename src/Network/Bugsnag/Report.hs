{-# LANGUAGE DeriveGeneric #-}
module Network.Bugsnag.Report
    ( BugsnagReport(..)
    , bugsnagReport
    ) where

import Data.Aeson
import Data.Aeson.Ext
import GHC.Generics
import Network.Bugsnag.Event
import Network.Bugsnag.Notifier

data BugsnagReport = BugsnagReport
    { brNotifier :: BugsnagNotifier
    , brEvents :: [BugsnagEvent]
    }
    deriving Generic

instance ToJSON BugsnagReport where
    toJSON = genericToJSON $ bsAesonOptions "br"
    toEncoding = genericToEncoding $ bsAesonOptions "br"

bugsnagReport :: [BugsnagEvent] -> BugsnagReport
bugsnagReport events = BugsnagReport
    { brNotifier = bugsnagNotifier
    , brEvents = events
    }
