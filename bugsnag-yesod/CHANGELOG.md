## [_Unreleased_](https://github.com/pbrisbin/bugsnag-haskell/compare/bugsnag-yesod-v1.0.1.0...main)

- None

## [v1.0.1.0](https://github.com/pbrisbin/bugsnag-haskell/compare/bugsnag-yesod-v1.0.0.1...v1.0.1.0)

- Adds support for `annotated-exception`

  The Bugsnag middleware avoids catching `HandlerContents`, because this particular
  exception represents Yesod control flow, not an exception that needs to be reported.
  With this change, it also avoids catching `AnnotatedException HandlerContents`.
  This preserves the correct behavior even if the `HandlerContents` exception ended
  up being rethrown by one of the utilities in the `annotated-exception` package.

## [v1.0.0.1](https://github.com/pbrisbin/bugsnag-haskell/compare/bugsnag-yesod-v1.0.0.0...bugsnag-yesod-v1.0.0.1)

- Support GHCs 9.0 and 9.2

## [v1.0.0.0](https://github.com/pbrisbin/bugsnag-haskell/tree/bugsnag-yesod-v1.0.0.0)

First released version.

---

For CHANGELOG details prior to the package re-organization, see
[`archive/CHANGELOG.md`](../archive/CHANGELOG.md).
