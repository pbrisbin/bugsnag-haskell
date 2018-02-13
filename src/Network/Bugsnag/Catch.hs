{-# LANGUAGE ScopedTypeVariables #-}
module Network.Bugsnag.Catch
    ( catchBugsnag
    ) where

import Control.Exception
import Network.Bugsnag.Exception
import Network.Bugsnag.Notify
import Network.Bugsnag.Settings

-- | Catch any exception, notify bugsnag and re-throw it
catchBugsnag :: IO a -> BugsnagSettings -> IO a
catchBugsnag io settings = io `catch` \ex -> do
    notifyBugsnag settings $ bugsnagExceptionFromSomeException ex
    throw $ toException ex
