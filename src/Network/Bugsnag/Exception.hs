{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Network.Bugsnag.Exception
    ( BugsnagException(..)
    , bugsnagException
    , bugsnagExceptionFromSomeException
    ) where

import Control.Exception
import Data.Aeson
import Data.Aeson.Ext
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (typeRep)
import GHC.Generics
import Instances.TH.Lift ()
import Network.Bugsnag.Exception.Parse
import Network.Bugsnag.StackFrame

-- | Opaque type for @'Exception' e => e -> 'BugsnagException'@
--
-- These can be placed in a heterogenious list and then tried in turn to find
-- something better than @'SomeException'@. This is a shameless copy of the
-- @'Handler'@ type (and general approach) used by @'catches'@.
--
data Caster = forall e. Exception e => Caster (e -> BugsnagException)

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
-- that's just silly. To include a stack frame from the location of construction
-- via Template Haskell, see @'currentStackFrame'@.
--
bugsnagException :: Text -> Text -> [BugsnagStackFrame] -> BugsnagException
bugsnagException errorClass message stacktrace = BugsnagException
    { beErrorClass = errorClass
    , beMessage = Just message
    , beStacktrace = stacktrace
    }

-- | Construct a @'BugsnagException'@ from a @'SomeException'@
--
-- @'BugsnagException'@s are left as-is, and @'ErrorCall'@ exceptions are parsed
-- for @'HasCallStack'@ information to use as @stacktrace@. Otherwise, we
-- attempt to determine @errorClass@ and we use the @'show'@n exception as
-- @message@.
--
-- >>> :m +System.IO.Error
-- >>> bugsnagExceptionFromSomeException $ toException $ userError "Oops"
-- BugsnagException {beErrorClass = "IOException", beMessage = Just "user error (Oops)", beStacktrace = []}
--
bugsnagExceptionFromSomeException :: SomeException -> BugsnagException
bugsnagExceptionFromSomeException ex =
    foldr go (bugsnagExceptionFromException ex) exCasters
  where
    go :: Caster -> BugsnagException -> BugsnagException
    go (Caster caster) res = maybe res caster $ fromException ex

exCasters :: [Caster]
exCasters =
    [ Caster id
    , Caster bugsnagExceptionFromErrorCall
    , Caster $ bugsnagExceptionFromException @IOException
    , Caster $ bugsnagExceptionFromException @ArithException
    , Caster $ bugsnagExceptionFromException @ArrayException
    , Caster $ bugsnagExceptionFromException @AssertionFailed
    , Caster $ bugsnagExceptionFromException @SomeAsyncException
    , Caster $ bugsnagExceptionFromException @AsyncException
    , Caster $ bugsnagExceptionFromException @NonTermination
    , Caster $ bugsnagExceptionFromException @NestedAtomically
    , Caster $ bugsnagExceptionFromException @BlockedIndefinitelyOnMVar
    , Caster $ bugsnagExceptionFromException @BlockedIndefinitelyOnSTM
    , Caster $ bugsnagExceptionFromException @AllocationLimitExceeded
    , Caster $ bugsnagExceptionFromException @Deadlock
    , Caster $ bugsnagExceptionFromException @NoMethodError
    , Caster $ bugsnagExceptionFromException @PatternMatchFail
    , Caster $ bugsnagExceptionFromException @RecConError
    , Caster $ bugsnagExceptionFromException @RecSelError
    , Caster $ bugsnagExceptionFromException @RecUpdError
    , Caster $ bugsnagExceptionFromException @TypeError
    ]

-- | Construct a @'BugsnagException'@ from an @'ErrorCall'@
--
-- This type of exception may have @'HasCallStack'@ information.
--
bugsnagExceptionFromErrorCall :: ErrorCall -> BugsnagException
bugsnagExceptionFromErrorCall ex =
    case parseErrorCall ex of
        Left _ -> bugsnagExceptionFromException ex
        Right (MessageWithStackFrames message stacktrace) ->
            bugsnagException (exErrorClass ex) message stacktrace

-- | Construct a @'BugsnagException'@ from an @'Exception'@
--
-- This exists mostly as a way to provide the type hint.
--
-- > bugsnagExceptionFromException @IOException ex
--
bugsnagExceptionFromException :: Exception e => e -> BugsnagException
bugsnagExceptionFromException ex =
    bugsnagException (exErrorClass ex) (T.pack $ show ex) []

-- | Show an exception's "error class"
--
-- >>> exErrorClass (undefined :: IOException)
-- "IOException"
--
-- >>> exErrorClass (undefined :: SomeException)
-- "SomeException"
--
exErrorClass :: forall e. Exception e => e -> Text
exErrorClass _ = T.pack $ show $ typeRep $ Proxy @e
