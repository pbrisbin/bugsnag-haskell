# Bugsnag Reporter

Bugsnag error reporter/notifier for Haskell applications.

## Configuration

```hs
settings <- newBugsnagSettings "BUGSNAG_API_KEY"
```

See [`Network.Bugsnag.Settings`](#todo).

## Reporting an Error

```hs
notifyBugsnag settings
  $ bugsnagException "Error" "message" [$(currentStackFrame) "myFunction"]
```

See [`Network.Bugsnag.Notify`](#todo).

## Throwing & Catching

Throw a `BugsnagException` yourself:

```hs
throw
  $ bugsnagException "Error" "message" [$(currentStackFrame) "myFunction"]
```

Catch any exceptions, notify, and re-throw it:

```hs
myFunction `catch` \ex -> do
  notifyBugsnag settings ex
  throw ex
```

## Examples

### Command-Line

```hs
module Main (main) where

import App (appMain) -- Actual program logic
import qualified Data.Text as T
import Network.Bugsnag

main :: IO ()
main = do
    apiKey <- BugsnagApiKey . T.pack <$> getEnv "BUGSNAG_API_KEY"
    settings <- newBugsnagSettings apiKey
    appMain `catchBugsnag` settings
```

### WAI / Warp

```hs
warpSettings :: BugsnagSettings -> Settings
warpSettings settings = setOnException
    (\mRequest ex ->
        when (defaultShouldDisplayException ex) $ do
            let beforeNotify = maybe id updateEventFromRequest mRequest

            void $ forkIO
                $ notifyBugsnagWith beforeNotify settings
                $ bugsnagExceptionFromSomeException ex

    ) defaultSettings
```

### Yesod

**NOTE**: the `yesodMiddleware` hook is the only way to handle things as actual
exceptions. The alternative, using `errorHandler`, means you would only ever see
`InternalError Text`. The main downside is that short-circuit responses also
come through the middleware as exceptions too, and must be filtered. (Unless of
course you *want* to notify Bugsnag of 404s and such.)

```hs
--
-- Foundation.hs
--
data App = App
    { -- ...
    , appBugsnag :: BugsnagSettings
    }

instance YesodApp where
    -- ...

    yesodMiddleware handler = do
        settings <- getsYesod appBugsnag

        -- Simple, synchronous, no request or session info:
        defaultYesodMiddleware handler `catchBugsnag` settings

        -- More complex, asynchronous, request info added:
        request <- waiRequest

        let beforeNotify = updateEventFromRequest
                $ bugsnagRequestFromWaiRequest request

        defaultYesodMiddleware handler `catch` \ex ->
            unless (isHandlerContents ex)
                $ void $ liftIO $ forkIO
                $ notifyBugsnagWith beforeNotify settings
                $ bugsnagExceptionFromSomeException ex

      where
        isHandlerContents :: SomeException -> Bool
        isHandlerContents ex =
            -- There's a million ways to do this...
            case (fromException ex :: Maybe HandlerContents) of
                Just _ -> True
                Nothing -> False

--
-- Application.hs
--
makeFoundation = do
    -- ...

    let appBugsnag = (bugsnagSettings "..." manager)
            { bsAppVersion = ...
            , bsReleaseStage = ...
            }

    pure App{..}
```

## Contributing

See [CONTRIBUTING](./CONTRIBUTING.md).

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)
