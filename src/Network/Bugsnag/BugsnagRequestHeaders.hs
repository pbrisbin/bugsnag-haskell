{-# LANGUAGE OverloadedStrings #-}
module Network.Bugsnag.BugsnagRequestHeaders
  ( BugsnagRequestHeaders
  , bugsnagRequestHeaders
  , bugsnagRequestHeadersToHeaders
  , mapBugsnagRequestHeaders
  , redactBugsnagRequestHeaders
  )
 where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types

-- | Wrapper around Wai's 'RequestHeaders', used to give a custom ToJSON instance.
newtype BugsnagRequestHeaders = BugsnagRequestHeaders {unBugsnagRequestHeaders :: RequestHeaders}
    deriving (Show, Eq, Ord)

instance ToJSON BugsnagRequestHeaders where
    toJSON (BugsnagRequestHeaders headers) =
        object $ map headerToText headers

        where
            -- Headers are most commonly ASCII, which UTF-8 is a superset of, so UTF-8 is fine to decode
            headerToText :: (CI ByteString, ByteString) -> (Text, Value)
            headerToText (name, value) = (TE.decodeUtf8 $ CI.original name, String $ TE.decodeUtf8 value)

-- | Create 'BugsnagRequestHeaders'
bugsnagRequestHeaders :: RequestHeaders -> BugsnagRequestHeaders
bugsnagRequestHeaders = BugsnagRequestHeaders

-- | Get 'RequestHeaders' from 'BugsnagRequestHeaders'
bugsnagRequestHeadersToHeaders :: BugsnagRequestHeaders -> RequestHeaders
bugsnagRequestHeadersToHeaders = unBugsnagRequestHeaders

-- | Apply a function over each header in BugsnagRequestHeaders
mapBugsnagRequestHeaders :: (Header -> Header) -> BugsnagRequestHeaders -> BugsnagRequestHeaders
mapBugsnagRequestHeaders fn (BugsnagRequestHeaders headers) = BugsnagRequestHeaders $ map fn headers

-- | For headers with the given names, replace their value with "<redacted>".
--
-- This is intended to remove sensitive data from headers.
redactBugsnagRequestHeaders :: [HeaderName] -> BugsnagRequestHeaders -> BugsnagRequestHeaders
redactBugsnagRequestHeaders redactList = mapBugsnagRequestHeaders redactHeader
  where
    redactHeader :: Header -> Header
    redactHeader (k, _) | k `elem` redactList = (k, "<redacted>")
    redactHeader h = h
