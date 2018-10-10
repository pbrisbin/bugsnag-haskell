## [*Unreleased*](https://github.com/pbrisbin/bugsnag-haskell/compare/v0.0.2.1...master)

None

## [v0.0.2.1](https://github.com/pbrisbin/bugsnag-haskell/compare/v0.0.2.0...v0.0.2.1)

- Set StackFrame as in-project based on the `CodeIndex` when present
- Correctly parse `throwString` exceptions with newlines in the message

## [v0.0.2.0](https://github.com/pbrisbin/bugsnag-haskell/compare/v0.0.1.3...v0.0.2.0)

- Add `CodeIndex` support

  Adds a Template Haskell function to read your project source into an in-memory
  Map structure to be set on your `BugsnagSettings` value. Then, as part of
  notification, we can attach lines of source context to the StackFrames we
  have. NOTE: This has memory-footprint implications, so is entirely opt-in.

  See https://github.com/pbrisbin/bugsnag-haskell/pull/39

## [v0.0.1.3](https://github.com/pbrisbin/bugsnag-haskell/compare/v0.0.1.2...v0.0.1.3)

- Redact sensitive request headers by default [#31](https://github.com/pbrisbin/bugsnag-haskell/issues/31)
- Deprecate Settings that should be `BeforeNotify` values [#32](https://github.com/pbrisbin/bugsnag-haskell/issues/32)
- `bugsnagShouldNotify` operates after any `BeforeNotify` changes

## [v0.0.1.2](https://github.com/pbrisbin/bugsnag-haskell/tree/v0.0.1.2)

- Make App Version an opaque `Text`, not a structured `Version` ([@MaxGabriel](https://github.com/pbrisbin/bugsnag-haskell/pull/29))

## [v0.0.1.1](https://github.com/pbrisbin/bugsnag-haskell/tree/v0.0.1.1)

First (pre-) released version.
