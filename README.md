# Bugsnag Reporter

Bugsnag error reporter/notifier for Haskell applications.

## Quick Start

Minimal example:

```hs
settings <- newBugsnagSettings "NOTIFIER_API_KEY"
notifyBugsnag settings $ bugsnagException "Error" []
```

Including a stack frame for the location of notification:

```hs
notifyBugsnag settings
    $ bugsnagException "Error" [$(currentStackFrame) "myFunction"]
```

*N.B.*: a `message` is optional in the reporting API (which I find odd), so it's
not required or given in the above examples. It can be provided via
record-update syntax and the `beMessage` field, but that can be cumbersome (it
requires clarifying parenthesis). Most higher-level APIs (see below) will accept
a message argument and do this for you.

## `BugsnagSettings`

*TODO*

## `notifyBugsnag`

*TODO*

## `notifyBugsnagWith`

*TODO*

## `BugsnagException`

*TODO*

## Throwing & Catching

*TODO*

### `throwBugsnag`

*TODO*

### `catchBugsnag`

*TODO*

## Web Applications

### App startup

### App error handler

### Point of error

## Library Design

This library aims to cover the _entire_ [reporting API][api-docs] with complete
types. The only things omitted are record fields specific to platforms that
won't (realistically) be written in Haskell (e.g. iOS).

[api-docs]: https://bugsnagerrorreportingapi.docs.apiary.io/#reference/0/notify/send-error-reports

The following enumerates all the possibilities:

```hs
fullySpecifiedReport :: BugsnagReport
fullySpecifiedReport = BugsnagReport
    { brNotifier = BugsnagNotifier
        { bnName
        , bnUrl
        , bnVersion
        }
    , brEvents =
        [ BugsnagEvent
            { beExceptions =
                [ BugsnagException
                    { beErrorClass
                    , beMessage
                    , beStacktrace =
                        [ BugsnagStackFrame
                            { bsfFile
                            , bsfLineNumber
                            , bsfColumnNumber
                            , bsfMethod
                            , bsfInProject
                            , bsfCode
                            }
                        ]
                    }
                ]
            , beBreadcrumbs = Just
                [ BugsnagBreadcrumb
                    { bbTimestamp
                    , bbName
                    , bbType
                    , bbMetaData
                    }
                ]
            , beRequest = Just BugsnagRequest
                { brClientIp
                , brHeaders
                , brHttpMethod
                , brUrl
                , brReferer
                }
            , beThreads = Just
                [ BugsnagThread
                    { btId
                    , btName
                    , btStacktrace
                    }
                ]
            , beContext
            , beGroupingHash
            , beUnhandled
            , beSeverity
            , beSeverityReason = Just BugsnagSeverityReason
                { bsrType
                , bsrAttributes = BugsnagSeverityReasonAttributes
                    { bsraErrorType
                    , bsraLevel
                    , bsraSignalType
                    , bsraViolationType
                    , bsraErrorClass
                    }
                }
            , beUser = Just BugsnagUser
                { buId
                , buEmailAddress
                , buUsername
                }
            , beApp = Just BugsnagApp
                { baId
                , baVersion
                , baBuildUUID
                , baReleaseStage
                , baType
                , baDsymUUIDs
                , baDuration
                , baDurationInForeground
                , baInForeground
                }
            , beDevice = Just BugsnagDevice { .. }
                { bdHostname
                , bdId
                , bdManufacturer
                , bdModel
                , bdModelNumber
                , bdOsName
                , bdOsVersion
                , bdFreeMemory
                , bdTotalMemory
                , bdFreeDisk
                , bdBrowserName
                , bdBrowserVersion
                , bdJailBroken
                , bdOrientation
                }
            , beMetaData = Just $ object [ ... ]
            }
        ]
    }
```

Please see the [Haddock documentation](#todo) for more details. The above may
fall out of date from time to time.

## Development & Tests

```console
stack setup
stack build --dependencies-only
stack build --pedantic --test
```

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)
