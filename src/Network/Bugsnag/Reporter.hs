{-# LANGUAGE OverloadedStrings #-}
-- |
--
-- Report an error to Bugsnag
--
-- <https://bugsnagerrorreportingapi.docs.apiary.io/#reference/0/notify/send-error-reports>
--
-- This is the lowest level interface, where you're required to provide all
-- payload data explicitly, as well as a TLS-enabled @'Manager'@
--
module Network.Bugsnag.Reporter
    ( reportError
    ) where

import Control.Monad (void)
import Data.Aeson
import Data.Text.Encoding (encodeUtf8)
import Network.Bugsnag.Event
import Network.Bugsnag.Notifier
import Network.Bugsnag.Settings
import Network.HTTP.Client
import Network.HTTP.Simple (setRequestBodyJSON, setRequestHeader)

reportError :: Manager -> BugsnagApiKey -> [BugsnagEvent] -> IO ()
reportError manager apiKey events = do
    request <- setupRequest <$> parseRequest "POST https://notify.bugsnag.com"
    void $ httpNoBody request manager
  where
    setupRequest
        = setRequestBodyJSON payload
        . setRequestHeader "Bugsnag-Api-Key" [key]
        . setRequestHeader "Bugsnag-Payload-Version" ["4"]

    key = encodeUtf8 $ unBugsnagApiKey apiKey
    payload = object ["notifier" .= bugsnagNotifier, "events" .= events]
