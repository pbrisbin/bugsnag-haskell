module Main
    ( main
    )
where

import Prelude

import Control.Exception (catch)
import Network.Bugsnag
import System.Exit (die)

main :: IO ()
main = do
    settings <- newBugsnagSettings "BUGSNAG_API_KEY"

    appMain `catch` \ex -> do
        notifyBugsnag settings ex
        die $ show ex

-- Actual program logic
appMain :: IO ()
appMain = error "Whoops"
