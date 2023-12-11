{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

module Main where

import           Prelude

import           Data.Bugsnag.Settings
import           Network.Bugsnag.Yesod
import           Network.Wai.Handler.Warp (run)
import           Yesod.Core

newtype App = App {appBugsnag :: Settings}
mkYesod "App" [parseRoutes|/ RootR GET|]

getRootR :: Handler Html
getRootR = error "Boom"

instance Yesod App where
  defaultLayout _widget = withUrlRenderer [hamlet|<html>|]
  yesodMiddleware =
    bugsnagYesodMiddleware appBugsnag . defaultYesodMiddleware

main :: IO ()
main = do
  let appBugsnag = defaultSettings "BUGSNAG_API_KEY"

  -- N.B. You should also consider setting Warp's onException
  run 3000 =<< toWaiApp App {..}
