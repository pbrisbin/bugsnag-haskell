{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Bugsnag.StackFrame
    ( BugsnagCode(..)
    , attachBugsnagCode
    , BugsnagStackFrame(..)
    , bugsnagStackFrame
    , currentStackFrame
    ) where

import Prelude

import Data.Aeson
import Data.Aeson.Ext
import Data.Text (Text)
import GHC.Generics
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax
import Network.Bugsnag.CodeIndex
import Numeric.Natural (Natural)

-- | Lines of code surrounding the error
--
-- Pairs of @(line-number, line-of-code)@, up to 3 on either side.
--
newtype BugsnagCode = BugsnagCode [(Natural, Text)]
    deriving newtype (Show, ToJSON)

-- | Attempt to attach a @'BugsnagCode'@ to a @'BugsnagStackFrame'@
--
-- Looks up the content in the Index by File/LineNumber and, if found, sets it
-- on the record.
--
attachBugsnagCode :: CodeIndex -> BugsnagStackFrame -> BugsnagStackFrame
attachBugsnagCode index sf =
    sf { bsfCode = findBugsnagCode (bsfFile sf) (bsfLineNumber sf) index }

findBugsnagCode :: FilePath -> Natural -> CodeIndex -> Maybe BugsnagCode
findBugsnagCode path n = fmap BugsnagCode . findSourceRange path (begin, n + 3)
  where
    begin
        | n < 3 = 0
        | otherwise = n - 3

data BugsnagStackFrame = BugsnagStackFrame
    { bsfFile :: FilePath
    , bsfLineNumber :: Natural
    , bsfColumnNumber :: Maybe Natural
    , bsfMethod :: Text -- ^ Function, in our parlance
    , bsfInProject :: Maybe Bool
    , bsfCode :: Maybe BugsnagCode
    }
    deriving stock (Generic, Show)

instance ToJSON BugsnagStackFrame where
    toJSON = genericToJSON $ bsAesonOptions "bsf"
    toEncoding = genericToEncoding $ bsAesonOptions "bsf"

bugsnagStackFrame :: FilePath -> Natural -> Text -> BugsnagStackFrame
bugsnagStackFrame path ln method = BugsnagStackFrame
    { bsfFile = path
    , bsfLineNumber = ln
    , bsfColumnNumber = Nothing
    , bsfMethod = method
    , bsfInProject = Nothing
    , bsfCode = Nothing
    }

-- | Construct a @'BugsnagStackFrame'@ from the point of this splice
--
-- Unfortunately there's no way to know the function, so that must be given:
--
currentStackFrame :: Q Exp
currentStackFrame = [|locStackFrame $(qLocation >>= liftLoc)|]

-- brittany-disable-next-binding

locStackFrame :: Loc -> Text -> BugsnagStackFrame
locStackFrame (Loc path _ _ (ls, cs) _) func =
    BugsnagStackFrame
        { bsfFile = path
        , bsfLineNumber = fromIntegral ls
        , bsfColumnNumber = Just $ fromIntegral cs
        , bsfMethod = func
        , bsfInProject = Just True
        -- N.B. this assumes we're unlikely to see adoption within libraries, or
        -- that such a thing would even work. If this function's used, it's
        -- assumed to be in end-user code.
        , bsfCode = Nothing
        }

-- brittany-disable-next-binding

-- Taken from monad-logger
liftLoc :: Loc -> Q Exp
liftLoc (Loc a b c (d1, d2) (e1, e2)) = [|Loc
    $(lift a)
    $(lift b)
    $(lift c)
    ($(lift d1), $(lift d2))
    ($(lift e1), $(lift e2))
    |]
