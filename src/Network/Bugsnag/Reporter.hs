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
    )
where

import Prelude

import Control.Monad (void)
import Data.Text.Encoding (encodeUtf8)
import Network.Bugsnag.Report
import Network.Bugsnag.Settings
import Network.HTTP.Client
import Network.HTTP.Simple (setRequestBodyJSON, setRequestHeader)

reportError :: Manager -> BugsnagApiKey -> BugsnagReport -> IO ()
reportError manager apiKey report = do
    request <- setupRequest <$> parseRequest "POST https://notify.bugsnag.com"
    void $ httpNoBody request manager
  where
    key = encodeUtf8 $ unBugsnagApiKey apiKey
    setupRequest =
        setRequestBodyJSON report
            . setRequestHeader "Bugsnag-Api-Key" [key]
            . setRequestHeader "Bugsnag-Payload-Version" ["4"]
