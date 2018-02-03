{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Bugsnag.Exception
    ( BugsnagException(..)
    , bugsnagException
    , bugsnagExceptionFromException
    , bugsnagExceptionFromErrorCall
    , bugsnagExceptionFromMessage
    ) where

import Control.Exception
import Data.Aeson
import Data.Aeson.Ext
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Instances.TH.Lift ()
import Network.Bugsnag.Exception.Parse
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

-- | Construct a @'BugsnagException'@ from an @'Exception'@
bugsnagExceptionFromException :: Exception e => Text -> e -> BugsnagException
bugsnagExceptionFromException errorClass ex = BugsnagException
    { beErrorClass = errorClass
    , beMessage = Just $ T.pack $ show ex
    , beStacktrace = []
    }

-- | Construct a @'BugsnagException'@ from an @'ErrorCall'@
--
-- This type of exception may have @'HasCallStack'@ information.
--
bugsnagExceptionFromErrorCall :: ErrorCall -> BugsnagException
bugsnagExceptionFromErrorCall e = case parseErrorCall e of
    Left _ -> bugsnagExceptionFromException "ErrorCall" e
    Right (MessageWithStackFrames message stacktrace) -> BugsnagException
        { beErrorClass = "ErrorCall"
        , beMessage = Just message
        , beStacktrace = stacktrace
        }

bugsnagExceptionFromMessage :: Text -> String -> BugsnagException
bugsnagExceptionFromMessage errorClass msg =
    case parseErrorCallMessage msg of
        Left _ -> (bugsnagException errorClass []) { beMessage = Just $ T.pack msg }
        Right (MessageWithStackFrames message stacktrace) -> BugsnagException
            { beErrorClass = errorClass
            , beMessage = Just message
            , beStacktrace = stacktrace
            }
