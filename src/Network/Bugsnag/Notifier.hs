{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
--
-- Static values about this notifier itself.
--
module Network.Bugsnag.Notifier
    ( BugsnagNotifier
    , bugsnagNotifier
    ) where

import Data.Aeson
import Data.Aeson.Ext
import Data.Text (Text)
import Data.Version
import GHC.Generics

data BugsnagNotifier = BugsnagNotifier
    { bnName :: Text
    , bnVersion :: Version
    , bnUrl :: Text
    }
    deriving Generic

instance ToJSON BugsnagNotifier where
    toJSON = genericToJSON $ lowerDroppingPrefix "bn"
    toEncoding = genericToEncoding $ lowerDroppingPrefix "bn"

bugsnagNotifier :: BugsnagNotifier
bugsnagNotifier = BugsnagNotifier
    { bnName = "haskell-bugsnag-reporter"
    , bnVersion = makeVersion [0, 0, 1, 0]
    , bnUrl = "https://github.com/pbrisbin/bugsnag-reporter"
    }
