module Main
    ( main
    ) where

import Prelude

import Control.Exception (SomeException, catch)
import Data.Bugsnag.Settings
import Network.Bugsnag
import System.Exit (die)

main :: IO ()
main = do
    let settings = defaultSettings "BUGSNAG_API_KEY"

    appMain `catch` \ex -> do
        notifyBugsnag @SomeException settings ex
        die $ show ex

-- Actual program logic
appMain :: IO ()
appMain = error "Whoops"
