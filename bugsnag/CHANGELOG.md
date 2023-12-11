## [_Unreleased_](https://github.com/pbrisbin/bugsnag-haskell/compare/bugsnag-v1.1.0.0...main)

- None

## [v1.0.1.0](https://github.com/pbrisbin/bugsnag-haskell/compare/bugsnag-v1.0.0.0...bugsnag-v1.1.0.0)

- New module: `Network.Bugsnag.MetaData`

- Adds some support for the `annotated-exception` package.
  `updateEventFromOriginalException` now catches either `e` or `AnnotatedException e`.

  - `bugsnagExceptionFromSomeException` now has special cases to handle
    `AnnotatedException` well.
  - Annotations of type `CallStack` and `MetaData` are included in the bugsnag
    report; other annotations are ignored.

- Adds explicit support for `StringException` from the `unliftio` package.

  - `bugsnagExceptionFromSomeException` now has special cases to handle
    `StringException` well.

## [v1.0.0.1](https://github.com/pbrisbin/bugsnag-haskell/compare/bugsnag-v1.0.0.0...bugsnag-v1.0.0.1)

- Support GHCs 9.0 and 9.2

## [v1.0.0.0](https://github.com/pbrisbin/bugsnag-haskell/tree/bugsnag-v1.0.0.0)

First released version.

---

For CHANGELOG details prior to the package re-organization, see
[`archive/CHANGELOG.md`](../archive/CHANGELOG.md).
