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
import qualified Data.Text as T
import Network.Bugsnag

main :: IO ()
main = do
    apiKey <- BugsnagApiKey . T.pack <$> getEnv "BUGSNAG_API_KEY"
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
            void $ liftIO $ forkIO
                $ notifyBugsnagWith beforeNotify settings
                $ bugsnagExceptionFromSomeException ex

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
