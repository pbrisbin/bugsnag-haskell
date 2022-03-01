# Bugsnag error reporter for Haskell

Catch exceptions in your Haskell code and report then to Bugsnag.

## Configuration

```hs
let settings = defaultSettings "A_BUGSNAG_API_KEY"
```

## Manual Reporting

`Data.Bugsnag.Exception` is the type of actual exceptions included in the event
reported to Bugsnag. Constructing it directly can be useful to attach the
current source location as a stack frame.

```hs
let
  ex = defaultException
    { exception_errorClass = "Error"
    , exception_message = Just "message"
    , exception_stacktrace = [$(currentStackFrame) "myFunction"]
    }
```

In order to treat it like an actual Haskell `Exception`, wrap it in
`AsException`:

```hs
notifyBugsnag settings $ AsException ex
```

## Catching & Throwing

Catch any exceptions, notify, and re-throw:

```hs
myFunction `withException` notifyBugsnag @SomeException settings
```

Throw a manually-built exception:

```hs
throwIO $ AsException ex
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

## `bugsnag-hs`

We depend on `bugsnag-hs` to define the types for the full reporting API
payload. Unfortunately, it exposes them from its own `Network.Bugsnag` module,
which conflicts with ourselves.

To get around this, we re-export that whole module as `Data.Bugsnag`. If you are
currently depending on `bugsnag-hs` and wish to use our package too, we
recommend you only depend on us and use its types through the `Data.Bugsnag`
re-export.

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)
