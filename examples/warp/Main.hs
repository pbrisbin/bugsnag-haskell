{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException)
import Control.Monad (void, when)
import Network.Bugsnag
import Network.Wai (Application, Request)
import Network.Wai.Handler.Warp

app :: Application
app = error "Oops"

warpSettings :: IO Settings
warpSettings = do
    settings <- newBugsnagSettings "BUGSNAG_API_KEY"

    pure
        $ setPort 3000
        $ setOnException (bugsnagOnException settings) defaultSettings

-- N.B. This may some day be provided by a bugsnag-warp package
bugsnagOnException :: BugsnagSettings -> Maybe Request -> SomeException -> IO ()
bugsnagOnException settings mRequest ex =
    when (defaultShouldDisplayException ex) $ do
        let beforeNotify = maybe id updateEventFromWaiRequest mRequest
        void $ forkIO $ notifyBugsnagWith beforeNotify settings ex

main :: IO ()
main = do
    settings <- warpSettings
    runSettings settings app
