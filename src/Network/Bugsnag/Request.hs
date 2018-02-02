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
import Data.Text (Text)
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

bugsnagRequest :: BugsnagRequest
bugsnagRequest = BugsnagRequest
    { brClientIp = Nothing
    , brHeaders = Nothing
    , brHttpMethod = Nothing
    , brUrl = Nothing
    , brReferer = Nothing
    }

bugsnagRequestFromWaiRequest :: Request -> BugsnagRequest
bugsnagRequestFromWaiRequest request = bugsnagRequest
    -- TODO: we might have to look for headers in the proxy case
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
