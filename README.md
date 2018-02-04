# Bugsnag Reporter

Bugsnag error reporter/notifier for Haskell applications.

## Quick Start

Minimal example:

```hs
settings <- newBugsnagSettings "NOTIFIER_API_KEY"
notifyBugsnag settings $ bugsnagException "Error" "message" []
```

Including a stack frame for the location of notification:

```hs
notifyBugsnag settings
    $ bugsnagException "Error" "message" [$(currentStackFrame) "myFunction"]
```

Modifying the Event before reporting it, e.g. to set a severity:

```hs
notifyBugsnagWith warningSeverity settings
    $ bugsnagException "Error" "message" [$(currentStackFrame) "myFunction"])
```

*NOTE: a global before-notify can be defined in settings too.*

## Throwing & Catching

A `BugsnagException` can be used with anything in
[`Control.Monad.Catch`][exceptions]:

[exceptions]: http://hackage.haskell.org/package/exceptions

```hs
throwM $ bugsnagException "Error" "message" []
```

```hs
possiblyErroringCode `catch` notifyBugsnag settings
```

This would be enough if `BugsnagException` exceptions were the only things ever
thrown in your applications. Since that's unlikely, there is `catchBugsnag`:

```hs
possiblyErroringCode `catchBugsnag` settings
```

This function catches all exceptions defined in `Control.Exception`, notifies
Bugsnag, then re-throws. It handles two cases specially:

1. A caught `BugsnagException` is notified as-is, hopefully with a `stacktrace`
1. A caught `ErrorCall` is checked for `HasCallStack` information, which ends up
   in the notified `stacktrace`

If you have exceptions outside of those in `Control.Exception`, and you don't
want them to come through with `SomeException` as their `errorClass`, you can
use `catchesBugsnag` to supply your own handlers which will run before ours:

```hs
(possiblyErroringCode `catchesBugsnag` settings)
    [ Handler (\(ex :: MyException) -> {- ... -})
    , Handler (\(ex :: AWSException) -> {- ... -})
    ]
```

## Settings

*TODO: document.*

## Requests, Sessions, Users, & Apps

*TODO: document.*

## Web Frameworks

The following uses Yesod as an example, but the ideas should apply to any
framework that has an obvious place for (1) constructing some app-wide state at
startup and (2) handling any per-request errors.

When handling request errors, add a notification to bugsnag:

```hs
errorHandler e@(InternalError msg) = do
    forkHandler $ do
        settings <- getsYesod appBugsnag
        notifyBugsnag settings "InternalError"
            $ bugsnagExceptionFromMessage
            $ T.unpack msg

    defaultErrorHandler e

errorHandler other = defaultErrorHandler other
```

At startup, set a `BugsnagSettings` value that's accessible as seen above:

```hs
let appBugsnag :: BugsnagSettings Handler
    appBugsnag = bugsnagSettings "..." manager
        { bsReleaseStage = ...
        , bsBeforeNotify = \event -> do
            request <- bugsnagRequestFromWaiRequest =<< waiRequest
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
