{-# LANGUAGE ExistentialQuantification #-}

module Network.Bugsnag.Exception
    ( AsException(..)
    , bugsnagExceptionFromSomeException
    ) where

import Prelude

import Control.Exception hiding (Exception)
import qualified Control.Exception as Exception
import Data.Bugsnag
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import Data.Text (Text, pack)
import Data.Typeable (typeRep)
import Instances.TH.Lift ()
import Network.Bugsnag.Exception.Parse

-- | Newtype over 'Exception', so it can be thrown and caught
newtype AsException = AsException
    { unAsException :: Exception
    }
    deriving newtype Show
    deriving anyclass Exception.Exception

-- | Construct a 'Exception' from a 'SomeException'
bugsnagExceptionFromSomeException :: SomeException -> Exception
bugsnagExceptionFromSomeException ex = fromMaybe fallback $ asum
    [ unAsException <$> fromException ex
    , bugsnagExceptionWithParser parseErrorCall <$> fromException ex
    ]
  where
    fallback = (bugsnagExceptionWithParser parseStringException ex)
        { exception_errorClass = (\(SomeException e) -> exErrorClass e) ex
        }

bugsnagExceptionWithParser
    :: Exception.Exception e
    => (e -> Either String MessageWithStackFrames)
    -> e
    -> Exception
bugsnagExceptionWithParser p ex = case p ex of
    Left _ -> bugsnagExceptionFromException ex
    Right (MessageWithStackFrames message stacktrace) -> defaultException
        { exception_errorClass = exErrorClass ex
        , exception_message = Just message
        , exception_stacktrace = stacktrace
        }

bugsnagExceptionFromException :: Exception.Exception e => e -> Exception
bugsnagExceptionFromException ex = defaultException
    { exception_errorClass = exErrorClass ex
    , exception_message = Just $ pack $ displayException ex
    , exception_stacktrace = []
    }

-- | Show an exception's "error class"
exErrorClass :: forall e . Exception.Exception e => e -> Text
exErrorClass _ = pack $ show $ typeRep $ Proxy @e
