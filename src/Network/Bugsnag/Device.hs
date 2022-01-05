module Network.Bugsnag.Device
    ( bugsnagDeviceFromWaiRequest
    ) where

import Prelude

import Data.Bugsnag
import Data.ByteString (ByteString)
import Data.Text (pack, unpack)
import Data.Version
import qualified Network.Wai as Wai
import Text.Read (readMaybe)
import Web.UAParser

-- | /Attempt/ to divine a 'Device' from a request's User Agent
bugsnagDeviceFromWaiRequest :: Wai.Request -> Maybe Device
bugsnagDeviceFromWaiRequest request = do
    userAgent <- lookup "User-Agent" $ Wai.requestHeaders request
    pure $ bugsnagDeviceFromUserAgent userAgent

-- |
--
-- >>> device = bugsnagDeviceFromUserAgent "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/64.0.3282.119 Safari/537.36"
-- >>> device_osName device
-- Just "Linux"
--
-- N.B. we always return a Device, it may just be lacking some or all fields.
--
-- >>> device_osVersion device
-- Nothing
--
-- >>> device_browserName device
-- Just "Chrome"
--
-- >>> device_browserVersion device
-- Just "64.0.3282"
--
bugsnagDeviceFromUserAgent :: ByteString -> Device
bugsnagDeviceFromUserAgent userAgent = defaultDevice
    { device_osName = osrFamily <$> osResult
    , device_osVersion = do
        result <- osResult
        v1 <- readMaybe . unpack =<< osrV1 result
        v2 <- readMaybe . unpack =<< osrV2 result
        v3 <- readMaybe . unpack =<< osrV3 result
        v4 <- readMaybe . unpack =<< osrV4 result
        pure $ pack $ showVersion $ makeVersion [v1, v2, v3, v4]
    , device_browserName = uarFamily <$> uaResult
    , device_browserVersion = do
        result <- uaResult
        v1 <- readMaybe . unpack =<< uarV1 result
        v2 <- readMaybe . unpack =<< uarV2 result
        v3 <- readMaybe . unpack =<< uarV3 result
        pure $ pack $ showVersion $ makeVersion [v1, v2, v3]
    }
  where
    uaResult = parseUA userAgent
    osResult = parseOS userAgent
