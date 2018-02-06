# Bugsnag Reporter

Bugsnag error reporter/notifier for Haskell applications.

## Report an Error

```hs
settings <- newBugsnagSettings "BUGSNAG_API_KEY"
notifyBugsnag settings $ bugsnagException
    "ErrorClass" "Some message" [$(currentStackFrame) "myFunction"]
```

## Throwing & Catching

Throw a `BugsnagException` yourself:

```hs
throwM $ bugsnagException
    "ErrorClass" "Some message" [$(currentStackFrame) "myFunction"]
```

And catch (only) it:

```hs
myFunction `catch` notifyBugsnag settings
```

Catch any exceptions, notify, and re-throw it:

```hs
myFunction `catchBugsnag` settings
```

See [`Network.Bugsnag.Catch`](#todo) for more details.

## Configuration

See [`Network.Bugsnag.Settings`](#todo) for details.

## Yesod

In `Foundation.hs`, your `Yesod` instance:

```hs
data App = App
  { -- ...
  , appBugsnag :: BugsnagSettings (HandlerT App IO)
  }

errorHandler e@(InternalError msg) = do
    forkHandler ($logErrorS "errorHandler" . tshow) $ do
        settings <- getsYesod appBugsnag
        notifyBugsnag settings
            $ bugsnagExceptionFromMessage "InternalError"
            $ T.unpack msg
    defaultErrorHandler e

errorHandler e = defaultErrorHandler e
```

In `Application.hs`, `makeFoundation`:

```hs
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

## Development & Tests

```console
stack setup
stack build --dependencies-only
stack build --pedantic --test
```

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)
