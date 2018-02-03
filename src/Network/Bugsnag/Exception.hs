{-# LANGUAGE DeriveGeneric #-}
module Network.Bugsnag.Exception
    ( BugsnagException(..)
    , bugsnagException
    ) where

import Control.Exception
import Data.Aeson
import Data.Aeson.Ext
import Data.Text (Text)
import GHC.Generics
import Instances.TH.Lift ()
import Network.Bugsnag.StackFrame

data BugsnagException = BugsnagException
    { beErrorClass :: Text
    , beMessage :: Maybe Text
    , beStacktrace :: [BugsnagStackFrame]
    }
    deriving (Generic, Show)

instance ToJSON BugsnagException where
    toJSON = genericToJSON $ lowerDroppingPrefix "be"
    toEncoding = genericToEncoding $ lowerDroppingPrefix "be"

instance Exception BugsnagException

-- | Construct a throwable @'BugsnagException'@
--
-- To include a stack frame from the location of construction via Template
-- Haskell, see @'currentStackFrame'@.
--
-- Note that Message is optional in the API so it's not required in this
-- function, even though that seems odd. Use record update syntax to add one:
--
-- >>> :set -XOverloadedStrings
-- >>> (bugsnagException "errorClass" []) { beMessage = Just "Some message" }
-- BugsnagException {beErrorClass = "errorClass", beMessage = Just "Some message", beStacktrace = []}
--
bugsnagException :: Text -> [BugsnagStackFrame] -> BugsnagException
bugsnagException errorClass stacktrace = BugsnagException
    { beErrorClass = errorClass
    , beMessage = Nothing
    , beStacktrace = stacktrace
    }
