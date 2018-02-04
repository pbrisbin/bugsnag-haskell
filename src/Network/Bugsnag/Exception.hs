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
-- Note that Message is optional in the API, but we consider it required because
-- that's just silly.
--
-- To include a stack frame from the location of construction via Template
-- Haskell, see @'currentStackFrame'@.
--
-- >>> :set -XOverloadedStrings
-- >>> bugsnagException "errorClass" "message" []
-- BugsnagException {beErrorClass = "errorClass", beMessage = Just "message", beStacktrace = []}
--
bugsnagException :: Text -> Text -> [BugsnagStackFrame] -> BugsnagException
bugsnagException errorClass message stacktrace = BugsnagException
    { beErrorClass = errorClass
    , beMessage = Just message
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
    Right (MessageWithStackFrames message stacktrace) ->
        bugsnagException "ErrorCall" message stacktrace

bugsnagExceptionFromMessage :: Text -> String -> BugsnagException
bugsnagExceptionFromMessage errorClass msg =
    case parseErrorCallMessage msg of
        Left _ -> bugsnagException errorClass (T.pack msg) []
        Right (MessageWithStackFrames message stacktrace) -> BugsnagException
            { beErrorClass = errorClass
            , beMessage = Just message
            , beStacktrace = stacktrace
            }
