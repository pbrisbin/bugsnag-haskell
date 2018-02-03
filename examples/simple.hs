{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Bugsnag

main :: IO ()
main = do
    settings <- newBugsnagSettings "xxx"
    notifyBugsnag settings $ bugsnagException "Error" []
