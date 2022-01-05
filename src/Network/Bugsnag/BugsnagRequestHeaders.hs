module Network.Bugsnag.BugsnagRequestHeaders
    ( redactBugsnagRequestHeaders
    ) where

import Prelude

import qualified Data.CaseInsensitive as CI
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types

-- | For headers with the given names, replace their value with "<redacted>".
--
-- This is intended to remove sensitive data from headers.
--
redactBugsnagRequestHeaders
    :: [HeaderName] -> HashMap Text Text -> HashMap Text Text
redactBugsnagRequestHeaders redactList = HashMap.mapWithKey go
  where
    go :: Text -> Text -> Text
    go k _ | any (`matchesHeaderName` k) redactList = "<redacted>"
    go _ v = v

matchesHeaderName :: HeaderName -> Text -> Bool
matchesHeaderName h = (h ==) . CI.mk . TE.encodeUtf8
