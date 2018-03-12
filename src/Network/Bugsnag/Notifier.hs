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

import qualified Paths_bugsnag_haskell as Pkg

data BugsnagNotifier = BugsnagNotifier
    { bnName :: Text
    , bnVersion :: Version
    , bnUrl :: Text
    }
    deriving Generic

instance ToJSON BugsnagNotifier where
    toJSON = genericToJSON $ bsAesonOptions "bn"
    toEncoding = genericToEncoding $ bsAesonOptions "bn"

bugsnagNotifier :: BugsnagNotifier
bugsnagNotifier = BugsnagNotifier
    { bnName = "bugsnag-haskell"
    , bnVersion = Pkg.version
    , bnUrl = "https://github.com/pbrisbin/bugsnag-haskell"
    }
