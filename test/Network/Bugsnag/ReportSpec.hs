{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Network.Bugsnag.ReportSpec
    ( spec
    ) where

import Test.Hspec

import Data.Aeson
import Data.Aeson.QQ (aesonQQ)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Network.Bugsnag

spec :: Spec
spec = do
    describe "JSON payload" $ do
        it "is right for a minimally-specified report" $ do
            let report =
                    bugsnagReport
                        [ bugsnagEvent $ bugsnagException
                            "errorClass"
                            "message"
                            [ bugsnagStackFrame "src/Foo/Bar.hs" 10 "myFunction"
                            ]
                        ]

            -- N.B. we don't worry about the notifier object since it would need
            -- updating on version bumps and is a simple, static value.
            toJSON report `shouldBe` [aesonQQ|
                {
                    "notifier": #{bugsnagNotifier},
                    "events": [{
                        "exceptions": [{
                            "errorClass": "errorClass",
                            "message": "message",
                            "stacktrace": [{
                                "file": "src/Foo/Bar.hs",
                                "lineNumber": 10.0,
                                "method": "myFunction"
                            }]
                        }]
                    }]
                }
            |]

        it "is right for a mostly-specified report" $ do
            now <- getCurrentTime
            let report =
                    BugsnagReport
                        { brNotifier = bugsnagNotifier
                        , brEvents =
                            [ BugsnagEvent
                                { beException = BugsnagException
                                    { beErrorClass = "errorClass"
                                    , beMessage = Just "message"
                                    , beStacktrace =
                                        [ BugsnagStackFrame
                                            { bsfFile = "src/Foo/Bar.hs"
                                            , bsfLineNumber = 10
                                            , bsfColumnNumber = Just 12
                                            , bsfMethod = "myFunction"
                                            , bsfInProject = Just True
                                            , bsfCode = Nothing
                                            }
                                        ]
                                    , beOriginalException = Nothing
                                    }
                                , beBreadcrumbs = Just
                                    [ bugsnagBreadcrumb now "here" NavigationBreadcrumb
                                    , bugsnagBreadcrumb now "there" NavigationBreadcrumb
                                    ]
                                , beRequest = Just bugsnagRequest
                                    { brClientIp = Just "127.0.0.1"
                                    , brHeaders = Just (BugsnagRequestHeaders [])
                                    , brHttpMethod = Just "POST"
                                    , brUrl = Just "https://example.com"
                                    }
                                , beThreads = Just
                                    [ bugsnagThread
                                        { btId = Just "1"
                                        , btName = Just "thread-1"
                                        }
                                    ]
                                , beContext = Just "app"
                                , beGroupingHash = Nothing
                                , beUnhandled = Just False
                                , beSeverity = Just WarningSeverity
                                , beSeverityReason = Just BugsnagSeverityReason
                                    { bsrType = UnhandledExceptionReasonType
                                    , bsrAttributes = bugsnagSeverityReasonAttributes
                                        { bsraErrorType = Just "reason"
                                        , bsraLevel = Just "1"
                                        , bsraSignalType = Just "SIGKILL"
                                        }
                                    }
                                , beUser = Just bugsnagUser
                                    { buId = Just "1"
                                    , buUsername = Just "pbrisbin"
                                    }
                                , beApp = Just bugsnagApp
                                    { baId = Just "1"
                                    , baVersion = Just "1.0.0"
                                    }
                                , beDevice = Nothing
                                , beMetaData = Just $ object
                                    [ "foo" .= ("bar" :: Text)
                                    , "baz" .= ("bat" :: Text)
                                    ]
                                }
                            ]
                        }

            toJSON report `shouldBe` [aesonQQ|
                {
                    "notifier": #{bugsnagNotifier},
                    "events": [{
                        "exceptions": [{
                            "errorClass": "errorClass",
                            "message": "message",
                            "stacktrace": [{
                                "file": "src/Foo/Bar.hs",
                                "lineNumber": 10,
                                "columnNumber": 12,
                                "method": "myFunction",
                                "inProject": true
                            }]
                        }],
                        "context": "app",
                        "breadcrumbs": [{
                            "name": "here",
                            "type": "navigation",
                            "timestamp": #{now}
                        }, {
                            "name": "there",
                            "type": "navigation",
                            "timestamp": #{now}
                        }],
                        "app": { "id": "1", "version": "1.0.0" },
                        "user": { "id": "1", "username": "pbrisbin" },
                        "severity": "warning",
                        "severityReason": {
                            "type": "unhandledException",
                            "attributes": {
                                "errorType": "reason",
                                "level": "1",
                                "signalType": "SIGKILL"
                            }
                        },
                        "threads": [{ "id": "1", "name": "thread-1" }],
                        "request": {
                            "clientIp": "127.0.0.1",
                            "httpMethod": "POST",
                            "url": "https://example.com",
                            "headers": {}
                        },
                        "unhandled": false,
                        "metaData": {
                            "foo": "bar",
                            "baz": "bat"
                        }
                    }]
                }
            |]
