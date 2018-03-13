# Bugsnag error reporter for Haskell

Catch and report exceptions in your Haskell code.

## Configuration

```hs
settings <- newBugsnagSettings "BUGSNAG_API_KEY"
```

See [`Network.Bugsnag.Settings`](http://hackage.haskell.org/package/bugsnag-haskell/docs/Network-Bugsnag-Settings.html).

## Reporting an Error

```hs
notifyBugsnag settings $ toException
    $ bugsnagException "Error" "message"
        [ $(currentStackFrame) "myFunction"
        ]
```

See [`Network.Bugsnag.Notify`](http://hackage.haskell.org/package/bugsnag-haskell/docs/Network-Bugsnag-Notify.html).

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
stack build --flag bugsnag-haskell:examples
```

## Contributing

See [CONTRIBUTING](./CONTRIBUTING.md).

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)
