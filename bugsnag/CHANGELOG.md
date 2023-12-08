## [_Unreleased_](https://github.com/pbrisbin/bugsnag-haskell/compare/bugsnag-v1.0.1.0...main)

- None

## [v1.0.1.0](https://github.com/pbrisbin/bugsnag-haskell/compare/bugsnag-v1.0.0.0...bugsnag-v1.0.1.0)

- Adds some support for the `annotated-exception` package.
  `updateEventFromOriginalException` now catches either `e` or `AnnotatedException e`.
  (This is technically a breaking change, but either it doesn't affect you
  or is most likely the behavior you want.)
  The new `updateEventFromOriginalAnnotatedException` utility can be used to inspect
  the original exception's annotations.

## [v1.0.0.1](https://github.com/pbrisbin/bugsnag-haskell/compare/bugsnag-v1.0.0.0...bugsnag-v1.0.0.1)

- Support GHCs 9.0 and 9.2

## [v1.0.0.0](https://github.com/pbrisbin/bugsnag-haskell/tree/bugsnag-v1.0.0.0)

First released version.

---

For CHANGELOG details prior to the package re-organization, see
[`archive/CHANGELOG.md`](../archive/CHANGELOG.md).
