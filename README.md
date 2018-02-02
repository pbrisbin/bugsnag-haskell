# Bugsnag Reporter

Bugsnag error reporter/notifier for Haskell applications.

## Usage

Notify immediately and directly:

```hs
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Network.Bugsnag

main :: IO ()
main = do
    settings <- newBugsnagSettings "NOTIFIER_API_KEY"
    notifyBugsnag settings bugsnagSession $(bugsnagException "main" "Oops")
```

Catch a thrown `BugsnagException` and notify:

```hs
main :: IO ()
main = do
    settings <- newBugsnagSettings "NOTIFIER_API_KEY"
    brokenFunction `catch` notifyBugsnag settings bugsnagSession

brokenFunction :: IO ()
brokenFunction = throwIO $(bugsnagException "brokenFunction" "Oops")
```

Notify of all 500s from a Yesod error-handler, including request and user
session information:

**NOTE: this example is aspirational, not yet implemented.**

```hs
customErrorHandler (InternalError e) = do
    $(logError) e

    -- Don't block the error response. Bugsnag should be best-effort
    forkHandler $ do
        request <- waiRequest
        session <- getBugsnagSession -- perhaps reading YesodAuth data
        settings <- appBugsnag <$> getsYesod appSettings

        liftIO
            $ notifyBugsnagEvent settings event
            $ updateEventFromSession session
            $ updateEventFromWaiRequest request
            $ bugsnagEvent [bugsnagExceptionFromError e]

    -- Rest of 500 response
```

## Library Design

This library aims to cover the _entire_ [reporting API][api-docs] with complete
types. The only things omitted are record fields specific to platforms that
won't (realistically) be written in Haskell (e.g. iOS).

[api-docs]: https://bugsnagerrorreportingapi.docs.apiary.io/#reference/0/notify/send-error-reports

Fields which are not required in the payload are typed `Maybe`. Same-named
constructor functions are provided which accept all non-`Maybe` fields
positionally and give back a value with `Nothing` everywhere else:

```hs
myEvent :: BugsnagEvent
myEvent = bugsnagEvent
    [ $(bugsnagException "errorClass" "errorMessage")
        { beSeverity = Just WarningSeverity
        }
    ]
```

It's recommend to use the constructor functions and override the fields via
record-update syntax. (Lenses would be a welcome addition, but are not a
priority for me right now.) The following enumerates all the possibilities, for
reference:

```hs
fullySpecified :: IO ()
fullySpecified = notifyBugsnagEvents
    ( BugsnagSettings
        { bsApiKey
        , bsHttpManager
        }
    )
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
        , beSeverity =
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
```

Please see the [Haddock documentation][#todo] for information on each type.

## Development & Tests

```console
stack setup
stack build --dependencies-only
stack build --pedantic --test
```

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)
