{-# LANGUAGE CPP #-}

module Data.Aeson.Compat
  ( -- * Key
    Key
  , fromText
  , toText

    -- * KeyMap
  , KeyMap
  , empty
  , null
  , singleton
  , fromList
  , toList
  , unionWith

    -- * Etc.
  , Pair
  , Value (Object)
  , Object
  , object
  , (.=)
  ) where

import Data.Aeson.Types (Object, Pair, Value (Object), object, (.=))
#if MIN_VERSION_aeson(2, 0, 0)
import Data.Aeson.Key (Key, fromText, toText)
import Data.Aeson.KeyMap (KeyMap, empty, fromList, null, singleton, toList, unionWith)
-- Avoid unused-packages (unordered-containers) warning for this path
import Data.HashMap.Strict ()
#else
import Prelude (id)

import Data.HashMap.Strict (HashMap, empty, fromList, null, singleton, toList, unionWith)
import Data.Text (Text)

type Key = Text
type KeyMap = HashMap Text

fromText :: Text -> Key
fromText = id

toText :: Key -> Text
toText = id
#endif
