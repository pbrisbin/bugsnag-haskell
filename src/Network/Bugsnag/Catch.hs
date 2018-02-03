{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.Bugsnag.Catch
    ( catchBugsnag
    , catchesBugsnag
    ) where

import Control.Exception hiding (Handler, catches)
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO)
import Network.Bugsnag.Exception
import Network.Bugsnag.Notify
import Network.Bugsnag.Settings

-- | Catch any exception, notify bugsnag and re-throw it
--
-- Special cases:
--
-- 1. @'BugsnagException'@ is (obviously) notified as-is
-- 2. @'ErrorCall'@ is parsed for @'HasCallStack'@ information
--
-- All other exceptions are notified with their name as @errorClass@ and their
-- @'show'@n value as @message@.
--
-- N.B. A @stacktrace@ will only be present if it was in an original
-- @'BugsnagException'@ or found via @'HasCallStack'@.
--
catchBugsnag :: (MonadIO m, MonadCatch m) => m a -> BugsnagSettings m -> m a
catchBugsnag io settings = (io `catchesBugsnag` settings) []

-- | Same, but use the provided handlers before our own
catchesBugsnag
    :: (MonadIO m, MonadCatch m)
    => m a -> BugsnagSettings m -> [Handler m a] -> m a
catchesBugsnag io settings handlers = io `catches` allHandlers
  where
    allHandlers = handlers ++ exceptionHandlers settings

exceptionHandlers
    :: (MonadIO m, MonadThrow m)
    => BugsnagSettings m -> [Handler m a]
exceptionHandlers settings =
    [ Handler (notifyBugsnagThrow settings)
    , Handler (notifyBugsnagThrow settings . bugsnagExceptionFromErrorCall)

    -- I feel like there should be a way for me to write:
    --
    -- > , Handler $ genericHandler @IOException
    -- > , Handler $ genericHandler @ArithException
    -- > , etc
    --
    -- and have it:
    --
    -- - Monomorphize the function via TypeApplications, but *also*
    -- - Use reflection to get type name as a String value, for errorClass
    --
    -- Instead, we just spell it out for now...
    --
    , Handler (\(ex :: IOException) -> genericHandler "IOException" ex)
    , Handler (\(ex :: ArithException) -> genericHandler "ArithException" ex)
    , Handler (\(ex :: ArrayException) -> genericHandler "ArrayException" ex)
    , Handler (\(ex :: AssertionFailed) -> genericHandler "AssertionFailed" ex)
    , Handler (\(ex :: SomeAsyncException) -> genericHandler "SomeAsyncException" ex)
    , Handler (\(ex :: AsyncException) -> genericHandler "AsyncException" ex)
    , Handler (\(ex :: NonTermination) -> genericHandler "NonTermination" ex)
    , Handler (\(ex :: NestedAtomically) -> genericHandler "NestedAtomically" ex)
    , Handler (\(ex :: BlockedIndefinitelyOnMVar) -> genericHandler "BlockedIndefinitelyOnMVar" ex)
    , Handler (\(ex :: BlockedIndefinitelyOnSTM) -> genericHandler "BlockedIndefinitelyOnSTM" ex)
    , Handler (\(ex :: AllocationLimitExceeded) -> genericHandler "AllocationLimitExceeded" ex)
    , Handler (\(ex :: Deadlock) -> genericHandler "Deadlock" ex)
    , Handler (\(ex :: NoMethodError) -> genericHandler "NoMethodError" ex)
    , Handler (\(ex :: PatternMatchFail) -> genericHandler "PatternMatchFail" ex)
    , Handler (\(ex :: RecConError) -> genericHandler "RecConError" ex)
    , Handler (\(ex :: RecSelError) -> genericHandler "RecSelError" ex)
    , Handler (\(ex :: RecUpdError) -> genericHandler "RecUpdError" ex)
    , Handler (\(ex :: TypeError) -> genericHandler "TypeError" ex)
    , Handler (\(ex :: SomeException) -> genericHandler "SomeException" ex)
    ]
  where
    genericHandler errorClass
        = notifyBugsnagThrow settings
        . bugsnagExceptionFromException errorClass
