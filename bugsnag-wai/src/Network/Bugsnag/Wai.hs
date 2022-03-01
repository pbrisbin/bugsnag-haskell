module Network.Bugsnag.Wai
    ( bugsnagOnException
    , updateEventFromWaiRequest
    , updateEventFromWaiRequestUnredacted
    , bugsnagRequestFromWaiRequest
    , bugsnagDeviceFromWaiRequest

    -- * Exported for testing
    , redactRequestHeaders
    , readForwardedFor
    ) where

import Prelude

import Control.Applicative ((<|>))
import Control.Arrow ((***))
import Control.Concurrent (forkIO)
import Control.Exception (SomeException)
import Control.Monad (void, when)
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
import qualified Data.Text.Encoding as TE
import Network.Bugsnag
import Network.Bugsnag.Device
import Network.HTTP.Types
import Network.Socket
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

bugsnagOnException :: Settings -> Maybe Wai.Request -> SomeException -> IO ()
bugsnagOnException =
    bugsnagOnExceptionWith (maybe mempty updateEventFromWaiRequest)

bugsnagOnExceptionWith
    :: (Maybe Wai.Request -> BeforeNotify)
    -> Settings
    -> Maybe Wai.Request
    -> SomeException
    -> IO ()
bugsnagOnExceptionWith mkBeforeNotify settings mRequest ex =
    when (Warp.defaultShouldDisplayException ex) $ do
        void $ forkIO $ notifyBugsnagWith (mkBeforeNotify mRequest) settings ex

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
sockAddrToIp _ = "<socket>"

-- | /Attempt/ to divine a 'Device' from a request's User Agent
bugsnagDeviceFromWaiRequest :: Wai.Request -> Maybe Device
bugsnagDeviceFromWaiRequest request = do
    userAgent <- lookup "User-Agent" $ Wai.requestHeaders request
    pure $ bugsnagDeviceFromUserAgent userAgent

-- | Set the events 'Event' and 'Device'
--
-- This function redacts the following Request headers:
--
-- - Authorization
-- - Cookie
-- - X-XSRF-TOKEN (CSRF token header used by Yesod)
--
-- To avoid this, use 'updateEventFromWaiRequestUnredacted'.
--
updateEventFromWaiRequest :: Wai.Request -> BeforeNotify
updateEventFromWaiRequest wrequest =
    redactRequestHeaders ["Authorization", "Cookie", "X-XSRF-TOKEN"]
        <> updateEventFromWaiRequestUnredacted wrequest

updateEventFromWaiRequestUnredacted :: Wai.Request -> BeforeNotify
updateEventFromWaiRequestUnredacted wrequest =
    let
        mdevice = bugsnagDeviceFromWaiRequest wrequest
        request = bugsnagRequestFromWaiRequest wrequest
    in maybe mempty setDevice mdevice <> setRequest request

-- | Redact the given request headers
--
-- Headers like @Authorization@ may contain information you don't want to report
-- to Bugsnag.
--
-- > redactRequestHeaders ["Authorization", "Cookie"]
--
redactRequestHeaders :: [HeaderName] -> BeforeNotify
redactRequestHeaders headers = updateEvent $ \event ->
    event { event_request = redactHeaders headers <$> event_request event }

redactHeaders :: [HeaderName] -> Request -> Request
redactHeaders headers request = request
    { request_headers = redactBugsnagRequestHeaders headers
        <$> request_headers request
    }

redactBugsnagRequestHeaders
    :: [HeaderName] -> HashMap Text Text -> HashMap Text Text
redactBugsnagRequestHeaders redactList = HashMap.mapWithKey go
  where
    go :: Text -> Text -> Text
    go k _ | any (`matchesHeaderName` k) redactList = "<redacted>"
    go _ v = v

matchesHeaderName :: HeaderName -> Text -> Bool
matchesHeaderName h = (h ==) . CI.mk . TE.encodeUtf8
