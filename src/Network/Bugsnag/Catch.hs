{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.Bugsnag.Catch
    ( catchBugsnag
    ) where

import Control.Concurrent.Async.Lifted (async)
import Control.Exception hiding (Handler, catch)
import Control.Monad (void)
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control
import Network.Bugsnag.Exception
import Network.Bugsnag.Notify
import Network.Bugsnag.Settings

-- | Catch any exception, notify bugsnag and re-throw it
--
-- Notification is best-effort, done asynchronously
--
catchBugsnag
    :: ( MonadBaseControl IO m
       , MonadIO m
       , MonadCatch m
       )
    => m a
    -> BugsnagSettings m -> m a
catchBugsnag io settings = io `catch` \ex -> do
    void $ async $ notifyBugsnag settings $ bugsnagExceptionFromSomeException ex
    throwM $ toException ex
