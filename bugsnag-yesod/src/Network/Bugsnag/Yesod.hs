{-# LANGUAGE AllowAmbiguousTypes #-}

-- | A 'yesodMiddleware' that notifies Bugsnag of exceptions
--
-- 'yesodMiddleware' is the only way to handle things as actual exceptions. The
-- alternative, using 'errorHandler', means you would only ever see  an
-- "InternalError Text" value.
--
-- The main downside to this middleware is that short-circuit responses also
-- come through the middleware as exceptions, and must be filtered. Unless of
-- course you want to notify Bugsnag of 404s and such.
--
module Network.Bugsnag.Yesod
    ( bugsnagYesodMiddleware
    , bugsnagYesodMiddlewareWith
    ) where

import Prelude

import Control.Exception.Annotated (AnnotatedException)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Bugsnag.Settings
import Data.Maybe (isJust)
import Network.Bugsnag
import Network.Bugsnag.Wai
import qualified Network.Wai as Wai
import UnliftIO.Exception
    (Exception, SomeException, fromException, withException)
import Yesod.Core (forkHandler, getsYesod, waiRequest)
import Yesod.Core.Types (HandlerContents, HandlerFor)

bugsnagYesodMiddleware
    :: (app -> Settings) -> HandlerFor app a -> HandlerFor app a
bugsnagYesodMiddleware = bugsnagYesodMiddlewareWith updateEventFromWaiRequest

bugsnagYesodMiddlewareWith
    :: (Wai.Request -> BeforeNotify)
    -> (app -> Settings)
    -> HandlerFor app a
    -> HandlerFor app a
bugsnagYesodMiddlewareWith mkBeforeNotify getSettings handler = do
    settings <- getsYesod getSettings
    request <- waiRequest

    handler `withException` \ex ->
        unless (isHandlerContents ex)
            $ forkHandler (const $ pure ())
            $ liftIO
            $ notifyBugsnagWith (mkBeforeNotify request) settings ex

isHandlerContents :: SomeException -> Bool
isHandlerContents ex =
    is @HandlerContents ex || is @(AnnotatedException HandlerContents) ex

is :: forall e . Exception e => SomeException -> Bool
is = isJust . fromException @e
