{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
module Main (main) where

import Control.Exception (SomeException, fromException, toException)
import Control.Monad (unless)
import Control.Monad.Catch (catch, throwM)
import Network.Bugsnag
import Network.Wai.Handler.Warp (run)
import Yesod.Core
import Yesod.Core.Types (HandlerContents)

newtype App = App { appBugsnag :: BugsnagSettings }
mkYesod "App" [parseRoutes|/ RootR GET|]

getRootR :: Handler Html
getRootR = error "Boom"

instance Yesod App where
    defaultLayout _widget = withUrlRenderer [hamlet|<html>|]

    -- N.B. yesodMiddleware is the only way to handle things as actual
    -- exceptions. The alternative, using errorHandler, means you would only
    -- ever see  an "InternalError Text" value.
    --
    -- The main downside to this middleware is that short-circuit responses also
    -- come through the middleware as exceptions, and must be filtered. Unless
    -- of course you want to notify Bugsnag of 404s and such.
    yesodMiddleware = bugsnagYesodMiddleware . defaultYesodMiddleware

-- This may be provided by a bugsnag-yesod package some day
bugsnagYesodMiddleware :: Handler a -> Handler a
bugsnagYesodMiddleware handler = do
    settings <- getsYesod appBugsnag
    request <- waiRequest

    handler `catch` \ex -> do
        unless (isHandlerContents ex)
            $ forkHandler (const $ pure ()) $ liftIO
            $ notifyBugsnagWith (updateEventFromWaiRequest request) settings ex
        throwM $ toException ex
  where
    isHandlerContents :: SomeException -> Bool
    isHandlerContents ex =
        case (fromException ex :: Maybe HandlerContents) of
            Just _ -> True
            Nothing -> False

main :: IO ()
main = do
    appBugsnag <- newBugsnagSettings "BUGSNAG_API_KEY"

    -- N.B. You should also consider setting Warp's onException
    run 3000 =<< toWaiApp App{..}
