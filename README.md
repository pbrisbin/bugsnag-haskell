# Bugsnag error reporter for Haskell

[![CI](https://github.com/pbrisbin/bugsnag-haskell/actions/workflows/ci.yml/badge.svg)](https://github.com/pbrisbin/bugsnag-haskell/actions/workflows/ci.yml)
[![Stackage nightly](https://github.com/pbrisbin/bugsnag-haskell/actions/workflows/nightly.yml/badge.svg)](https://github.com/pbrisbin/bugsnag-haskell/actions/workflows/nightly.yml)

Catch exceptions in your Haskell code and report then to Bugsnag.

## Configuration

```hs
let settings = defaultSettings "A_BUGSNAG_API_KEY"
```

See
[`Network.Bugsnag.Settings`](http://hackage.haskell.org/package/bugsnag-haskell/docs/Network-Bugsnag-Settings.html).

## Building an Error to Report

`Data.Bugsnag.Exception` is the type of actual exceptions included in the event
reported to Bugsnag. Constructing it directly can be useful to attach the
current source location as a stack frame.

```hs
let ex defaultException
        { exception_errorClass = "Error"
        , exception_message = Just "message"
        , exception_stacktrace = [$(currentStackFrame) "myFunction"]
        }
```

In order to treat it like an actual Haskell `Exception` wrap it in
`AsException`:

```hs
notifyBugsnag settings $ AsException ex
```

See
[`Network.Bugsnag.Exception`](http://hackage.haskell.org/package/bugsnag-haskell/docs/Network-Bugsnag-Exception.html).

## Throwing & Catching

```hs
throwIO $ AsException ex
```

Catch any exceptions, notify, and re-throw:

```hs
myFunction `withException` notifyBugsnag @SomeException settings
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
