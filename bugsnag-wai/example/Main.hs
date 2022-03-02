module Main
    ( main
    ) where

import Prelude

import qualified Data.Bugsnag.Settings as Bugsnag
import Network.Bugsnag.Wai (bugsnagOnException)
import Network.Wai (Application)
import Network.Wai.Handler.Warp

main :: IO ()
main = do
    settings <- warpSettings
    runSettings settings app

warpSettings :: IO Settings
warpSettings = do
    let settings = Bugsnag.defaultSettings "BUGSNAG_API_KEY"

    pure $ setPort 3000 $ setOnException
        (bugsnagOnException settings)
        defaultSettings

app :: Application
app = error "Oops"
