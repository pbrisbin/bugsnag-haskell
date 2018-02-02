# Bugsnag Reporter

Bugsnag error reporter/notifier for Haskell applications.

## Usage

### Simple

```hs
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Network.Bugsnag

testNotifyBugsnag :: IO ()
testNotifyBugsnag = do
    settings <- newBugsnagSettings "NOTIFIER_API_KEY"
    notifyBugsnag settings bugsnagSession $(bugsnagException "userError" "Oops")
```

### Rescued Exception

_TODO_

### Rescued Asynchronous Exception (`HasCallstack`)

_TODO_

### Yesod Error Handler

_TODO: maybe separate package._

### Wai Middleware

_TODO: maybe separate package._

## Design

### Types

This library aims to cover the entire [reporting API][api-docs] with types. It
enforces sum types for enumerations and record types for all objects. Object
fields are `Maybe` for non-required values. All defaulting is left to the
server, so consult the documentation if omitting fields.

[api-docs]: https://bugsnagerrorreportingapi.docs.apiary.io/#reference/0/notify/send-error-reports

We try to use descriptive types (e.g. `Natural` for values that must be
non-negative) but make concessions for infrequently used fields that lack
available types with `ToJSON` instances (e.g. we use `Text` not `Version`).

### Notify Interface

The interface for notifying is in terms of these types. Therefore, at the
boundaries of this package, one must construct full `BugsnagException`s,
`BugsnagSession`s, and so on. Functions are provided to make this easy in the
common case.

This means you would be required to build your stacktraces at this level.
Functions are again provided to make this easy in the context of from `Reader`
environment or `HasCallstack` function. See [TODO][#].

In _very_ stable and common context, such as a Yesod application, separate
packages are available to build these values for you out of information readily
at hand. See [TODO][#].

## Development & Tests

```console
stack setup
stack build --dependencies-only
stack build --pedantic --test
```

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)
