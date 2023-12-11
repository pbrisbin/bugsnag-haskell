{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module Network.Bugsnag.Exception
  ( AsException (..)
  , bugsnagExceptionFromSomeException
  ) where

import Prelude

import Control.Exception
  ( SomeException (SomeException)
  , displayException
  , fromException
  )
import qualified Control.Exception as Exception
import Control.Exception.Annotated
  ( AnnotatedException (AnnotatedException)
  , annotatedExceptionCallStack
  )
import qualified Control.Exception.Annotated as Annotated
import Data.Bugsnag
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Proxy (..), Typeable, typeRep)
import GHC.Stack (CallStack, SrcLoc (..), getCallStack)
import Network.Bugsnag.Exception.Parse
import UnliftIO.Exception (StringException (StringException))

-- | Newtype over 'Exception', so it can be thrown and caught
newtype AsException = AsException
  { unAsException :: Exception
  }
  deriving newtype (Show)
  deriving anyclass (Exception.Exception)

-- | Construct a 'Exception' from a 'SomeException'
bugsnagExceptionFromSomeException :: SomeException -> Exception
bugsnagExceptionFromSomeException ex =
  fromMaybe defaultException $
    asum
      [ bugsnagExceptionFromAnnotatedAsException <$> fromException ex
      , bugsnagExceptionFromStringException <$> fromException ex
      , bugsnagExceptionFromAnnotatedStringException <$> fromException ex
      , bugsnagExceptionFromAnnotatedException <$> fromException ex
      ]

-- | Respect 'AsException' as-is without modifications.
--   If it's wrapped in 'AnnotatedException', ignore the annotations.
bugsnagExceptionFromAnnotatedAsException
  :: AnnotatedException AsException -> Exception
bugsnagExceptionFromAnnotatedAsException = unAsException . Annotated.exception

-- | When a 'StringException' is thrown, we use its message and trace.
bugsnagExceptionFromStringException :: StringException -> Exception
bugsnagExceptionFromStringException (StringException message stack) =
  defaultException
    { exception_errorClass = typeName @StringException
    , exception_message = Just $ T.pack message
    , exception_stacktrace = callStackToStackFrames stack
    }

-- | When 'StringException' is wrapped in 'AnnotatedException',
--   there are two possible sources of a 'CallStack'.
--   Prefer the one from 'AnnotatedException', falling back to the
--   'StringException' trace if no 'CallStack' annotation is present.
bugsnagExceptionFromAnnotatedStringException
  :: AnnotatedException StringException -> Exception
bugsnagExceptionFromAnnotatedStringException ae@AnnotatedException {exception = StringException message stringExceptionStack} =
  defaultException
    { exception_errorClass = typeName @StringException
    , exception_message = Just $ T.pack message
    , exception_stacktrace =
        maybe
          (callStackToStackFrames stringExceptionStack)
          callStackToStackFrames
          $ annotatedExceptionCallStack ae
    }

-- | For an 'AnnotatedException' exception, derive the error class and message
--   from the wrapped exception.
--   If a 'CallStack' annotation is present, use that as the stacetrace.
--   Otherwise, attempt to parse a trace from the underlying exception.
bugsnagExceptionFromAnnotatedException
  :: AnnotatedException SomeException -> Exception
bugsnagExceptionFromAnnotatedException ae =
  case annotatedExceptionCallStack ae of
    Just stack ->
      defaultException
        { exception_errorClass = exErrorClass $ Annotated.exception ae
        , exception_message =
            Just $ T.pack $ displayException $ Annotated.exception ae
        , exception_stacktrace = callStackToStackFrames stack
        }
    Nothing ->
      let parseResult =
            asum
              [ fromException (Annotated.exception ae)
                  >>= (either (const Nothing) Just . parseErrorCall)
              , either (const Nothing) Just $
                  parseStringException (Annotated.exception ae)
              ]
      in  defaultException
            { exception_errorClass =
                exErrorClass $
                  Annotated.exception ae
            , exception_message =
                asum
                  [ mwsfMessage <$> parseResult
                  , Just $
                      T.pack $
                        displayException $
                          Annotated.exception
                            ae
                  ]
            , exception_stacktrace = foldMap mwsfStackFrames parseResult
            }

-- | Unwrap the 'SomeException' newtype to get the actual underlying type name
exErrorClass :: SomeException -> Text
exErrorClass (SomeException (_ :: e)) = typeName @e

typeName :: forall a. Typeable a => Text
typeName = T.pack $ show $ typeRep $ Proxy @a

-- | Converts a GHC call stack to a list of stack frames suitable
--   for use as the stacktrace in a Bugsnag exception
callStackToStackFrames :: CallStack -> [StackFrame]
callStackToStackFrames = fmap callSiteToStackFrame . getCallStack

callSiteToStackFrame :: (String, SrcLoc) -> StackFrame
callSiteToStackFrame (str, loc) =
  defaultStackFrame
    { stackFrame_method = T.pack str
    , stackFrame_file = T.pack $ srcLocFile loc
    , stackFrame_lineNumber = srcLocStartLine loc
    , stackFrame_columnNumber = Just $ srcLocStartCol loc
    }
