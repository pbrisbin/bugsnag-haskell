{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
--
-- Orphan instances and shared @'Generic'@ JSON options.
--
module Data.Aeson.Ext
    ( fromText
    , bsAesonOptions
    ) where

import Prelude

import Data.Aeson
import Data.Char (toLower)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)

#if MIN_VERSION_aeson(2,0,0)
import Data.Aeson.Key (fromText)
#else
import Data.Text (Text)
fromText :: Text -> Text
fromText = id
#endif

-- | Our custom Aeson @'Options'@
--
-- Omits @'Nothing'@ fields, and drops/lowers accordingly:
--
-- >>> fieldLabelModifier (bsAesonOptions "bs") "bsReleaseStage"
-- "releaseStage"
--
-- For sums, the first argument is taken as a suffix:
--
-- >>> constructorTagModifier (bsAesonOptions "ReasonType") "UnhandledExceptionReasonType"
-- "unhandledException"
--
bsAesonOptions :: String -> Options
bsAesonOptions prefixOrSuffix = defaultOptions
    { fieldLabelModifier = lowerFirst . dropPrefix prefixOrSuffix
    , constructorTagModifier = lowerFirst . dropSuffix prefixOrSuffix
    , omitNothingFields = True
    }

dropPrefix :: String -> String -> String
dropPrefix prefix x = fromMaybe x $ stripPrefix prefix x

dropSuffix :: String -> String -> String
dropSuffix prefix = reverse . dropPrefix (reverse prefix) . reverse

lowerFirst :: String -> String
lowerFirst [] = []
lowerFirst (x : rest) = toLower x : rest
