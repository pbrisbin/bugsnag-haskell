{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Control.Exception (toException)
import Network.Bugsnag

main :: IO ()
main = do
    settings <- newBugsnagSettings "BUGSNAG_API_KEY"

    notifyBugsnag settings $ toException
        $ bugsnagException "Error" "message"
            [ $(currentStackFrame) "myFunction"
            ]
