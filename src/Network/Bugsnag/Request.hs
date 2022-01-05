{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Network.Bugsnag.Request
    ( bugsnagRequestFromWaiRequest
    ) where

import Prelude

import Control.Applicative ((<|>))
import Control.Arrow ((***))
import Data.Bugsnag
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.CaseInsensitive as CI
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IP
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types
import Network.Socket
import qualified Network.Wai as Wai

-- | Constructs a 'Request' from a 'Wai.Request'
bugsnagRequestFromWaiRequest :: Wai.Request -> Request
bugsnagRequestFromWaiRequest request = defaultRequest
    { request_clientIp = decodeUtf8 <$> clientIp
    , request_headers = Just $ fromRequestHeaders $ Wai.requestHeaders request
    , request_httpMethod = Just $ decodeUtf8 $ Wai.requestMethod request
    , request_url = Just $ decodeUtf8 $ requestUrl request
    , request_referer = decodeUtf8 <$> Wai.requestHeaderReferer request
    }
  where
    clientIp =
        requestRealIp request <|> Just (sockAddrToIp $ Wai.remoteHost request)

fromRequestHeaders :: [(HeaderName, ByteString)] -> HashMap Text Text
fromRequestHeaders =
    HashMap.fromList . map (decodeUtf8 . CI.original *** decodeUtf8)

requestRealIp :: Wai.Request -> Maybe ByteString
requestRealIp request = requestForwardedFor request
    <|> lookup "X-Real-IP" (Wai.requestHeaders request)

requestForwardedFor :: Wai.Request -> Maybe ByteString
requestForwardedFor request =
    readForwardedFor =<< lookup "X-Forwarded-For" (Wai.requestHeaders request)

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

requestUrl :: Wai.Request -> ByteString
requestUrl request =
    requestProtocol
        <> "://"
        <> requestHost request
        <> prependIfNecessary "/" (Wai.rawPathInfo request)
        <> Wai.rawQueryString request
  where
    clientProtocol :: ByteString
    clientProtocol = if Wai.isSecure request then "https" else "http"

    requestHost :: Wai.Request -> ByteString
    requestHost = fromMaybe "<unknown>" . Wai.requestHeaderHost

    requestProtocol :: ByteString
    requestProtocol =
        fromMaybe clientProtocol
            $ lookup "X-Forwarded-Proto"
            $ Wai.requestHeaders request

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
