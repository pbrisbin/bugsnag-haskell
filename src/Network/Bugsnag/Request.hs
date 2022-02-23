{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Network.Bugsnag.Request
    ( BugsnagRequest(..)
    , bugsnagRequest
    , bugsnagRequestFromWaiRequest
    ) where

import Prelude

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Ext
import Data.Aeson.Types
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.IP
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
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
    deriving stock Generic

instance ToJSON BugsnagRequest where
    toJSON BugsnagRequest {..} = object
        ( "clientIp" .=? (decodeUtf8 <$> brClientIp)
        <> "headers" .=? brHeaders
        <> "httpMethod" .=? (decodeUtf8 <$> brHttpMethod)
        <> "url" .=? (decodeUtf8 <$> brUrl)
        <> "referer" .=? (decodeUtf8 <$> brReferer)
        )
      where
        -- For implementing "omit Nothing fields"
        (.=?) :: ToJSON v => Text -> Maybe v -> [Pair]
        (.=?) k = maybe [] (pure . (fromText k .=))
    toEncoding BugsnagRequest {..} = pairs
        ( "clientIp" .=? (decodeUtf8 <$> brClientIp)
        <> "headers" .=? brHeaders
        <> "httpMethod" .=? (decodeUtf8 <$> brHttpMethod)
        <> "url" .=? (decodeUtf8 <$> brUrl)
        <> "referer" .=? (decodeUtf8 <$> brReferer)
        )
      where
        -- For implementing "omit Nothing fields"
        (.=?) :: ToJSON v => Text -> Maybe v -> Series
        k .=? mv = maybe mempty (\v -> fromText k .= v) mv

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
        <> prependIfNecessary "/" (rawPathInfo request)
        <> rawQueryString request
  where
    clientProtocol :: ByteString
    clientProtocol = if isSecure request then "https" else "http"

    requestHost :: Request -> ByteString
    requestHost = fromMaybe "<unknown>" . requestHeaderHost

    requestProtocol :: ByteString
    requestProtocol =
        fromMaybe clientProtocol $ lookup "X-Forwarded-Proto" $ requestHeaders
            request

    prependIfNecessary c x
        | c `C8.isPrefixOf` x = x
        | otherwise = c <> x

sockAddrToIp :: SockAddr -> ByteString
sockAddrToIp (SockAddrInet _ h) = C8.pack $ show $ fromHostAddress h
sockAddrToIp (SockAddrInet6 _ _ h _) = C8.pack $ show $ fromHostAddress6 h
sockAddrToIp (SockAddrUnix _) = "<socket>"

-- Matches deprecated and eventually removed SockAddrCan on older GHCs.
-- overlapping-patterns warning is disabled for this.
sockAddrToIp _ = "<invalid>"
