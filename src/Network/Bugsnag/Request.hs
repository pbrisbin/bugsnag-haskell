{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
module Network.Bugsnag.Request
    ( BugsnagRequest(..)
    , bugsnagRequest
    , bugsnagRequestFromWaiRequest
    )
where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Ext
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.IP
import Data.Maybe (fromMaybe)
import GHC.Generics
import Network.Bugsnag.BugsnagRequestHeaders
import Network.HTTP.Types
import Network.Socket
import Network.Wai

-- | The web request being handled when the error was encountered
data BugsnagRequest = BugsnagRequest
    { brClientIp :: Maybe ByteString
    , brHeaders :: Maybe BugsnagRequestHeaders
    , brHttpMethod :: Maybe Method
    , brUrl :: Maybe ByteString
    , brReferer :: Maybe ByteString
    }
    deriving Generic

instance ToJSON BugsnagRequest where
    toJSON = genericToJSON $ bsAesonOptions "br"
    toEncoding = genericToEncoding $ bsAesonOptions "br"

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
    { brClientIp = requestRealIp request
        <|> Just (sockAddrToIp $ remoteHost request)
    , brHeaders = Just $ bugsnagRequestHeaders $ requestHeaders request
    , brHttpMethod = Just $ requestMethod request
    , brUrl = Just $ requestUrl request
    , brReferer = requestHeaderReferer request
    }

requestRealIp :: Request -> Maybe ByteString
requestRealIp request =
    requestForwardedFor request <|> lookup "X-Real-IP" (requestHeaders request)

requestForwardedFor :: Request -> Maybe ByteString
requestForwardedFor request =
    readForwardedFor =<< lookup "X-Forwarded-For" (requestHeaders request)

-- |
--
-- >>> readForwardedFor ""
-- Nothing
--
-- >>> readForwardedFor "123.123.123"
-- Just "123.123.123"
--
-- >>> readForwardedFor "123.123.123, 45.45.45"
-- Just "123.123.123"
--
readForwardedFor :: ByteString -> Maybe ByteString
readForwardedFor bs
    | C8.null bs = Nothing
    | otherwise = Just $ fst $ C8.break (== ',') bs

requestUrl :: Request -> ByteString
requestUrl request =
    requestProtocol
        <> "://"
        <> requestHost request
        <> rawPathInfo request
        <> rawQueryString request
  where
    clientProtocol = if isSecure request then "https" else "http"
    requestHost = fromMaybe "<unknown>" . requestHeaderHost
    requestProtocol =
        fromMaybe clientProtocol $ lookup "X-Forwarded-Proto" $ requestHeaders
            request

sockAddrToIp :: SockAddr -> ByteString
sockAddrToIp (SockAddrInet _ h) = C8.pack $ show $ fromHostAddress h
sockAddrToIp (SockAddrInet6 _ _ h _) = C8.pack $ show $ fromHostAddress6 h
sockAddrToIp (SockAddrUnix _) = "<socket>"

-- Matches deprecated and eventually removed SockAddrCan on older GHCs.
-- overlapping-patterns warning is disabled for this.
sockAddrToIp _ = "<invalid>"
