module Data.Aeson.Ext
    ( lowerDroppingPrefix
    ) where

import Data.Aeson (Options(..), defaultOptions)
import Data.Char (toLower)
import Data.List (stripPrefix)

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
