{-# LANGUAGE TemplateHaskell #-}

module Main
    ( main
    ) where

import Prelude

import Data.Bugsnag
import Data.Bugsnag.Settings
import Network.Bugsnag
import Network.Bugsnag.Exception
import Network.Bugsnag.StackFrame

main :: IO ()
main = do
    let settings = defaultSettings "BUGSNAG_API_KEY"

    notifyBugsnag settings $ AsException $ defaultException
        { exception_errorClass = "Error"
        , exception_message = Just "message"
        , exception_stacktrace = [$(currentStackFrame) "myFunction"]
        }
