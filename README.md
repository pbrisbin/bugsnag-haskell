# Bugsnag

Bugsnag Error Reporting from Haskell.

[![CI](https://github.com/pbrisbin/bugsnag-haskell/actions/workflows/ci.yml/badge.svg)](https://github.com/pbrisbin/bugsnag-haskell/actions/workflows/ci.yml)

## Packages

- [`bugsnag`](./bugsnag/README.md): Reporting library
- [`bugsnag-wai`](./bugsnag-wai/README.md): Integration for WAI (and Warp)
- [`bugsnag-yesod`](./bugsnag-yesod/README.md): Integration for Yesod

See individual `README`'s for details.

## Release

To trigger automated release, make a [conventionally-formatted
commit][conventional-commits]. Use package name as scope to have that commit
only cause release of that package. Use any other scope (or no scope) to release
all packages. A not-conventionally formatted commit will not trigger any
release.

Examples

- `chore: something` - no release
- `fix(bugsnag): something` - release `bugsnag` with minor version bump
- `fix!: something` - release all packages, each with a major version bump

[conventional-commits]: https://www.conventionalcommits.org/en/v1.0.0/
