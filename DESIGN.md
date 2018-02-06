## API Coverage

https://bugsnagerrorreportingapi.docs.apiary.io/#reference/0/notify/send-error-reports

```hs
BugsnagSettings
    { bsApiKey
    , bsAppVersion
    , bsReleaseStage
    , bsNotifyReleaseStages
    , bsBeforeNotify
    , bsGroupingHash
    , bsIsInProject
    , bsFilterStackFrames
    , bsHttpManager
    }

BugsnagSession
    { bsUser = Just BugsnagUser
        { buId
        , buEmailAddress
        , buUsername
        }
    , bsContext
    , bsMetaData = Just $ object [...]
    }

BugsnagReport
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

## Minimum Required Payload

```hs
bugsnagReport
    [ bugsnagEvent
        [ bugsnagException
            "errorClass"
            "message"
            [ bugsnagStackFrame "src/Foo/Bar.hs" 10 "myFunction"
            ]
        ]
    ]
```
