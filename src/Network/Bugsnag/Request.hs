{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Bugsnag.Request
    ( BugsnagRequest(..)
    , bugsnagRequest
    , bugsnagRequestFromWaiRequest
    ) where

import Data.Aeson
import Data.Aeson.Ext
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import GHC.Generics
import Network.HTTP.Types
import Network.Socket
import Network.Wai

-- | The web request being handled when the error was encountered
data BugsnagRequest = BugsnagRequest
    { brClientIp :: Maybe SockAddr
    , brHeaders :: Maybe RequestHeaders
    , brHttpMethod :: Maybe Method
    , brUrl :: Maybe ByteString
    , brReferer :: Maybe ByteString
    }
    deriving Generic

instance ToJSON BugsnagRequest where
    toJSON = genericToJSON $ lowerDroppingPrefix "br"
    toEncoding = genericToEncoding $ lowerDroppingPrefix "br"

-- | Constructs an empty @'BugsnagRequest'@
bugsnagRequest :: BugsnagRequest
bugsnagRequest = BugsnagRequest
    { brClientIp = Nothing
    , brHeaders = Nothing
    , brHttpMethod = Nothing
    , brUrl = Nothing
    , brReferer = Nothing
    }

-- | Constructs a @'BugsnagRequest'@ from a WAI @'Request'@
bugsnagRequestFromWaiRequest :: Request -> BugsnagRequest
bugsnagRequestFromWaiRequest request = bugsnagRequest
    { brClientIp = Just $ remoteHost request
    , brHeaders = Just $ requestHeaders request
    , brHttpMethod = Just $ requestMethod request
    , brUrl = Just requestUrl
    , brReferer = requestHeaderReferer request
    }
  where
    requestHost = fromMaybe "<unknown>" . requestHeaderHost
    requestUrl = "://"
        <> requestHost request
        <> rawPathInfo request
        <> rawQueryString request
