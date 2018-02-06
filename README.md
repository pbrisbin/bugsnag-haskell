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
throwM
  $ bugsnagException "Error" "message" [$(currentStackFrame) "myFunction"]
```

Catch (only) it:

```hs
myFunction `catch` notifyBugsnag settings
```

Catch any exceptions, notify, and re-throw it:

```hs
myFunction `catchBugsnag` settings
```

See [`Network.Bugsnag.Catch`](#todo).

## Examples

### Command-Line

```hs
module Main (main) where

import App (appMain) -- Actual program logic
import Network.Bugsnag

main :: IO ()
main = do
    apiKey <- BugsnagApiKey <$> getEnv "BUGSNAG_API_KEY"
    settings <- newBugsnagSettings apiKey
    appMain `catchBugsnag` settings
```

### Yesod

```hs
--
-- Foundation.hs
--
data App = App
  { -- ...
  , appBugsnag :: BugsnagSettings (HandlerT App IO)
  }

instance YesodApp where
    -- ...

    errorHandler e@(InternalError msg) = do
        forkHandler ($logErrorS "errorHandler" . tshow) $ do
            settings <- getsYesod appBugsnag
            notifyBugsnag settings
                $ bugsnagExceptionFromMessage "InternalError"
                $ T.unpack msg
        defaultErrorHandler e

    errorHandler e = defaultErrorHandler e

--
-- Application.hs
--
makeFoundation = do
    -- ...

    let appBugsnag :: BugsnagSettings Handler
        appBugsnag = (bugsnagSettings "..." manager)
            { bsAppVersion = ...
            , bsReleaseStage = ...
            , bsBeforeNotify = \event -> do
                request <- bugsnagRequestFromWaiRequest <$> waiRequest
                session <- getBugsnagSession -- e.g. using Yesod.Auth stuff

                pure
                    $ updateEventFromRequest request
                    $ updateEventFromSession session event
            }

    pure App{..}
```

## Contributing

See [CONTRIBUTING](./CONTRIBUTING.md).

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)
