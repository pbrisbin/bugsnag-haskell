{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Aeson.Ext
    ( lowerDroppingPrefix
    ) where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Char (toLower)
import Data.IP
import Data.List (stripPrefix)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Network.Socket

instance ToJSON ByteString where
    toJSON = String . decodeUtf8

instance ToJSON a => ToJSON (CI a) where
    toJSON = toJSON . CI.original

instance ToJSON SockAddr where
    toJSON (SockAddrInet port host) = String $ tshowHost host port
    toJSON (SockAddrInet6 port _ host _) = String $ tshowHost6 host port
    toJSON (SockAddrUnix p) = String $ T.pack $ "unix://" <> p
    toJSON (SockAddrCan x) = String $ T.pack $ "://" <> show x

-- |
--
-- >>> tshowHost 0x0100007f 8800
-- "127.0.0.1:8800"
--
tshowHost :: HostAddress -> PortNumber -> Text
tshowHost h p = T.pack $ show (fromHostAddress h) <> ":" <> show p

-- |
--
-- >>> tshowHost6 (0, 0, 0, 1) 8080
-- "::1:8080"
--
tshowHost6 :: HostAddress6 -> PortNumber -> Text
tshowHost6 h p = T.pack $ show (fromHostAddress6 h) <> ":" <> show p

-- | Convert fields as we need (the name's not quite right)
--
-- >>> fieldLabelModifier (lowerDroppingPrefix "bs") "bsFooBar"
-- "fooBar"
--
-- >>> fieldLabelModifier (lowerDroppingPrefix "bs") "oopsOops"
-- "oopsOops"
--
lowerDroppingPrefix :: String -> Options
lowerDroppingPrefix prefix = defaultOptions
    { fieldLabelModifier = \field ->
        case stripPrefix prefix field of
            Just (c:rest) -> toLower c : rest
            _ -> field

    }
