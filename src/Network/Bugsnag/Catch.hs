{-# LANGUAGE ScopedTypeVariables #-}
module Network.Bugsnag.Catch
    ( catchBugsnag
    ) where

import Control.Exception hiding (Handler, catch)
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO)
import Network.Bugsnag.Exception
import Network.Bugsnag.Notify
import Network.Bugsnag.Settings

-- | Catch any exception, notify bugsnag and re-throw it
catchBugsnag :: (MonadIO m, MonadCatch m) => m a -> BugsnagSettings m -> m a
catchBugsnag io settings = io `catch` \ex -> do
    notifyBugsnag settings $ bugsnagExceptionFromSomeException ex
    throwM $ toException ex
