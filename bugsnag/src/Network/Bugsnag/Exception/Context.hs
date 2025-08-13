{-# LANGUAGE CPP #-}

module Network.Bugsnag.Exception.Context
  ( ExceptionWithContext
  , displayExceptionWithContext
  ) where

import Prelude

#if MIN_VERSION_base(4,20,0)
import Control.Exception (Exception, ExceptionWithContext(..), displayException)
import Control.Exception.Context ( displayExceptionContext)

displayExceptionWithContext :: Exception e => ExceptionWithContext e -> String
displayExceptionWithContext (ExceptionWithContext anns e) =
  displayException e <> "\n" <> displayExceptionContext anns
#else
import Control.Exception (Exception, displayException)

newtype ExceptionWithContext e = ExceptionWithContext e
  deriving newtype (Eq, Show, Exception)

displayExceptionWithContext :: Exception e => ExceptionWithContext e -> String
displayExceptionWithContext (ExceptionWithContext ex) = displayException ex
#endif
