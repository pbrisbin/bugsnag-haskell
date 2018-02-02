{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.Bugsnag.Exception
    ( BugsnagStackFrame(..)
    , bugsnagStackFrame
    , BugsnagException(..)
    , bugsnagException
    ) where

import Control.Exception
import Data.Aeson
import Data.Aeson.Ext
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax
import Numeric.Natural (Natural)

-- | Lines of code surrounding the error
--
-- Pairs of @(line-number, line-of-code)@, up to 3 on either side.
--
newtype BugsnagCode = BugsnagCode [(Natural, Text)]
    deriving (Show, ToJSON)

data BugsnagStackFrame = BugsnagStackFrame
    { bsfFile :: FilePath
    , bsfLineNumber :: Natural
    , bsfColumnNumber :: Maybe Natural
    , bsfMethod :: Text -- ^ Function, in our parlance
    , bsfInProject :: Maybe Bool
    , bsfCode :: Maybe BugsnagCode
    }
    deriving (Generic, Show)

instance ToJSON BugsnagStackFrame where
    toJSON = genericToJSON $ lowerDroppingPrefix "bsf"
    toEncoding = genericToEncoding $ lowerDroppingPrefix "bsf"

bugsnagStackFrame :: FilePath -> Natural -> Text -> BugsnagStackFrame
bugsnagStackFrame path ln method = BugsnagStackFrame
    { bsfFile = path
    , bsfLineNumber = ln
    , bsfColumnNumber = Nothing
    , bsfMethod = method
    , bsfInProject = Nothing
    , bsfCode = Nothing
    }

data BugsnagException = BugsnagException
    { beErrorClass :: Text
    , beMessage :: Maybe Text
    , beStacktrace :: [BugsnagStackFrame]
    }
    deriving (Generic, Show)

instance ToJSON BugsnagException where
    toJSON = genericToJSON $ lowerDroppingPrefix "be"
    toEncoding = genericToEncoding $ lowerDroppingPrefix "be"

instance Exception BugsnagException

-- | Construct a throwable exception, taking location information into account
--
-- Such a value can be notified directly, or thrown and notified later.
--
-- > main :: IO
-- > main =
-- >     notifyBugsnag settings session $(bugsnagException "errorCall" "Oops")
--
-- It's expected you have an outer handler for this exception type.
--
-- > myFunc :: IO a
-- > myFunc = throwIO $(bugsnagException "myFunc" "Uh-oh")
-- >
-- > main :: IO ()
-- > main = myFunc `catch` notifyBugsnag settings session
--
bugsnagException :: Text -> Text -> Q Exp
bugsnagException errorClass errorMessage = [|
    BugsnagException
        { beErrorClass = $(lift errorClass)
        , beMessage = Just $(lift errorMessage)
        , beStacktrace = [locStackFrame $(qLocation >>= liftLoc)]
        }
    |]

locStackFrame :: Loc -> BugsnagStackFrame
locStackFrame (Loc path _ md (ls, cs) _) =
    BugsnagStackFrame
        { bsfFile = path
        , bsfLineNumber = fromIntegral ls
        , bsfColumnNumber = Just $ fromIntegral cs
        , bsfMethod = T.pack md
        , bsfInProject = Just True
        , bsfCode = Nothing
        }

-- Taken from monad-logger
liftLoc :: Loc -> Q Exp
liftLoc (Loc a b c (d1, d2) (e1, e2)) = [|Loc
    $(lift a)
    $(lift b)
    $(lift c)
    ($(lift d1), $(lift d2))
    ($(lift e1), $(lift e2))
    |]
