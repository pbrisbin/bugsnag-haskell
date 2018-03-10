# Bugsnag Reporter

Bugsnag error reporter/notifier for Haskell applications.

## Configuration

```hs
settings <- newBugsnagSettings "BUGSNAG_API_KEY"
```

See [`Network.Bugsnag.Settings`](#todo).

## Reporting an Error

```hs
notifyBugsnag settings $ toException
    $ bugsnagException "Error" "message"
        [ $(currentStackFrame) "myFunction"
        ]
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

- [Simple](./examples/simple/Main.hs)
- [Command-Line](./examples/cli/Main.hs)
- [WAI/Warp](./examples/warp/Main.hs)
- [Yesod](./examples/yesod/Main.hs)

Examples can be built locally with:

```console
stack build --flag bugsnag-reporter:examples
```

## Contributing

See [CONTRIBUTING](./CONTRIBUTING.md).

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)
