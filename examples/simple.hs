{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Exception (catch, throwIO)
import Network.Bugsnag

main :: IO ()
main = do
    settings <- newBugsnagSettings "b240eccd3a4b8fe14e221ef62c00a58f"
    brokenFunction `catch` notifyBugsnag settings bugsnagSession

brokenFunction :: IO ()
brokenFunction = throwIO $(bugsnagException "brokenFunction" "Oops")
