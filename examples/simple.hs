{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Exception (catch, throwIO)
import Network.Bugsnag

main :: IO ()
main = do
    settings <- newBugsnagSettings "xxx"
    brokenFunction `catch` notifyBugsnag settings bugsnagSession

brokenFunction :: IO ()
brokenFunction = throwIO $(bugsnagException "brokenFunction" "Oops")
