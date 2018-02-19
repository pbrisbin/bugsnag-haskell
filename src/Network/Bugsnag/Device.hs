{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Bugsnag.Device
    ( Bytes(..)
    , BugsnagDevice(..)
    , bugsnagDevice
    , bugsnagDeviceFromWaiRequest
    ) where

import Data.Aeson
import Data.Aeson.Ext
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Version
import GHC.Generics
--import Network.HTTP.Types
import Network.Wai
import Numeric.Natural
import Text.Read (readMaybe)
import Web.UAParser

newtype Bytes = Bytes Natural deriving ToJSON

data BugsnagDevice = BugsnagDevice
    { bdHostname :: Maybe Text
    , bdId :: Maybe Text
    , bdManufacturer :: Maybe Text
    , bdModel :: Maybe Text
    , bdModelNumber :: Maybe Text
    , bdOsName :: Maybe Text
    , bdOsVersion :: Maybe Version
    , bdFreeMemory :: Maybe Bytes
    , bdTotalMemory :: Maybe Bytes
    , bdFreeDisk :: Maybe Bytes
    , bdBrowserName :: Maybe Text
    , bdBrowserVersion :: Maybe Version
    , bdJailBroken :: Maybe Bool
    , bdOrientation :: Maybe Text
    }
    deriving Generic

instance ToJSON BugsnagDevice where
    toJSON = genericToJSON $ bsAesonOptions "bd"
    toEncoding = genericToEncoding $ bsAesonOptions "bd"

bugsnagDevice :: BugsnagDevice
bugsnagDevice = BugsnagDevice
    { bdHostname = Nothing
    , bdId = Nothing
    , bdManufacturer = Nothing
    , bdModel = Nothing
    , bdModelNumber = Nothing
    , bdOsName = Nothing
    , bdOsVersion = Nothing
    , bdFreeMemory = Nothing
    , bdTotalMemory = Nothing
    , bdFreeDisk = Nothing
    , bdBrowserName = Nothing
    , bdBrowserVersion = Nothing
    , bdJailBroken = Nothing
    , bdOrientation = Nothing
    }

-- | /Attempt/ to divine a @'BugsnagDevice'@ from a request's User Agent
bugsnagDeviceFromWaiRequest :: Request -> Maybe BugsnagDevice
bugsnagDeviceFromWaiRequest request = do
    userAgent <- lookup "User-Agent" $ requestHeaders request
    pure $ bugsnagDeviceFromUserAgent userAgent

-- |
--
-- >>> device = bugsnagDeviceFromUserAgent "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/64.0.3282.119 Safari/537.36"
-- >>> bdOsName device
-- Just "Linux"
--
-- N.B. we always return a Device, it may just be lacking some or all fields.
--
-- >>> showVersion <$> bdOsVersion device
-- Nothing
--
-- >>> bdBrowserName device
-- Just "Chrome"
--
-- >>> showVersion <$> bdBrowserVersion device
-- Just "64.0.3282"
--
bugsnagDeviceFromUserAgent :: ByteString -> BugsnagDevice
bugsnagDeviceFromUserAgent userAgent = bugsnagDevice
    { bdOsName = osrFamily <$> osResult
    , bdOsVersion = do
        result <- osResult
        v1 <- readMaybe . T.unpack =<< osrV1 result
        v2 <- readMaybe . T.unpack =<< osrV2 result
        v3 <- readMaybe . T.unpack =<< osrV3 result
        v4 <- readMaybe . T.unpack =<< osrV4 result
        pure $ makeVersion [v1, v2, v3, v4]
    , bdBrowserName = uarFamily <$> uaResult
    , bdBrowserVersion = do
        result <- uaResult
        v1 <- readMaybe . T.unpack =<< uarV1 result
        v2 <- readMaybe . T.unpack =<< uarV2 result
        v3 <- readMaybe . T.unpack =<< uarV3 result
        pure $ makeVersion [v1, v2, v3]
    }
  where
    uaResult = parseUA userAgent
    osResult = parseOS userAgent
