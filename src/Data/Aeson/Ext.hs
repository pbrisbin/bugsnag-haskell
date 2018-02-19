{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
--
-- Orphan instances and shared @'Generic'@ JSON options.
--
module Data.Aeson.Ext
    ( lowerDroppingPrefix
    ) where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Char (toLower)
import Data.List (stripPrefix)
import Data.Text.Encoding (decodeUtf8)

instance ToJSON ByteString where
    toJSON = String . decodeUtf8

instance ToJSON a => ToJSON (CI a) where
    toJSON = toJSON . CI.original

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
    , omitNothingFields = True
    }
