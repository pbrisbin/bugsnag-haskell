module Network.Bugsnag.BugsnagRequestHeaders
    ( BugsnagRequestHeaders
    , bugsnagRequestHeaders
    , redactBugsnagRequestHeaders
    ) where

import Prelude

import Data.Aeson
import Data.Aeson.Ext
import Data.ByteString (ByteString)
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types

-- | Wrapper around Wai's 'RequestHeaders', for custom 'ToJSON' instance
newtype BugsnagRequestHeaders = BugsnagRequestHeaders
    { unBugsnagRequestHeaders :: RequestHeaders
    }
    deriving stock (Show, Eq, Ord)

instance ToJSON BugsnagRequestHeaders where
    toJSON = object . map headerToKeyValue . unBugsnagRequestHeaders
    toEncoding = pairs . foldMap headerToKeyValue . unBugsnagRequestHeaders

headerToKeyValue :: KeyValue kv => (CI ByteString, ByteString) -> kv
headerToKeyValue (name, value) =
    fromText (TE.decodeUtf8 (CI.original name)) .= String (TE.decodeUtf8 value)

-- | Create 'BugsnagRequestHeaders'
bugsnagRequestHeaders :: RequestHeaders -> BugsnagRequestHeaders
bugsnagRequestHeaders = BugsnagRequestHeaders

-- | For headers with the given names, replace their value with "<redacted>".
--
-- This is intended to remove sensitive data from headers.
redactBugsnagRequestHeaders
    :: [HeaderName] -> BugsnagRequestHeaders -> BugsnagRequestHeaders
redactBugsnagRequestHeaders redactList = mapBugsnagRequestHeaders redactHeader
  where
    redactHeader :: Header -> Header
    redactHeader (k, _) | k `elem` redactList = (k, "<redacted>")
    redactHeader h = h

mapBugsnagRequestHeaders
    :: (Header -> Header) -> BugsnagRequestHeaders -> BugsnagRequestHeaders
mapBugsnagRequestHeaders fn (BugsnagRequestHeaders headers) =
    BugsnagRequestHeaders $ map fn headers
