{-# LANGUAGE ExistentialQuantification #-}

module Network.Bugsnag.Exception
    ( BugsnagException(..)
    , bugsnagException
    , bugsnagExceptionFromSomeException
    ) where

import Prelude

import Control.Exception
import Data.Aeson
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
data Caster = forall e . Exception e => Caster (e -> BugsnagException)

data BugsnagException = BugsnagException
    { beErrorClass :: Text
    , beMessage :: Maybe Text
    , beStacktrace :: [BugsnagStackFrame]
    , beOriginalException :: Maybe SomeException
    }
    deriving stock (Generic, Show)

instance ToJSON BugsnagException where
    toJSON BugsnagException {..} = object
        [ "errorClass" .= beErrorClass
        , "message" .= beMessage
        , "stacktrace" .= beStacktrace
        ]

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
    , beOriginalException = Nothing
    }

-- | Construct a @'BugsnagException'@ from a @'SomeException'@
--
-- @'BugsnagException'@s are left as-is, and @'ErrorCall'@ exceptions are parsed
-- for @'HasCallStack'@ information to use as @stacktrace@. Otherwise, we
-- attempt to determine @errorClass@ and we use @'displayException'@ as
-- @message@.
--
-- >>> import Control.Arrow
-- >>> import System.IO.Error
-- >>> (beErrorClass &&& beMessage) $ bugsnagExceptionFromSomeException $ toException $ userError "Oops"
-- ("IOException",Just "user error (Oops)")
--
bugsnagExceptionFromSomeException :: SomeException -> BugsnagException
bugsnagExceptionFromSomeException ex = foldr go seed exCasters
  where
    go :: Caster -> BugsnagException -> BugsnagException
    go (Caster caster) res = maybe res caster $ fromException ex

    seed = (bugsnagExceptionWithParser parseStringException ex)
        { beErrorClass = (\(SomeException e) -> exErrorClass e) ex
        }

exCasters :: [Caster]
exCasters = [Caster id, Caster $ bugsnagExceptionWithParser parseErrorCall]

bugsnagExceptionWithParser
    :: Exception e
    => (e -> Either String MessageWithStackFrames)
    -> e
    -> BugsnagException
bugsnagExceptionWithParser p ex = case p ex of
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
    (bugsnagException (exErrorClass ex) (T.pack $ displayException ex) [])
        { beOriginalException = Just $ toException ex
        }

-- | Show an exception's "error class"
--
-- >>> exErrorClass (undefined :: IOException)
-- "IOException"
--
-- >>> exErrorClass (undefined :: SomeException)
-- "SomeException"
--
exErrorClass :: forall e . Exception e => e -> Text
exErrorClass _ = T.pack $ show $ typeRep $ Proxy @e
