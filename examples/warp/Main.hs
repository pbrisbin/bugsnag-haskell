module Main
    ( main
    ) where

import Prelude

import Control.Concurrent (forkIO)
import Control.Exception (SomeException)
import Control.Monad (void, when)
import Network.Bugsnag hiding (Settings, defaultSettings)
import qualified Network.Bugsnag as Bugsnag
import Network.Wai (Application, Request)
import Network.Wai.Handler.Warp

app :: Application
app = error "Oops"

warpSettings :: IO Settings
warpSettings = do
    let settings = Bugsnag.defaultSettings "BUGSNAG_API_KEY"

    pure $ setPort 3000 $ setOnException
        (bugsnagOnException settings)
        defaultSettings

-- N.B. This may some day be provided by a bugsnag-warp package
bugsnagOnException
    :: Bugsnag.Settings -> Maybe Request -> SomeException -> IO ()
bugsnagOnException settings mRequest ex =
    when (defaultShouldDisplayException ex) $ do
        let bn = maybe mempty updateEventFromWaiRequest mRequest
        void $ forkIO $ notifyBugsnagWith bn settings ex

main :: IO ()
main = do
    settings <- warpSettings
    runSettings settings app
