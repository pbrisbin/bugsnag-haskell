{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Bugsnag.StackFrame
    ( attachBugsnagCode
    , currentStackFrame
    ) where

import Prelude

import Data.Bugsnag
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text, pack, unpack)
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax
import Network.Bugsnag.CodeIndex

-- | Attempt to attach code to a 'StackFrame'
--
-- Looks up the content in the Index by File/LineNumber and, if found, sets it
-- on the record.
--
attachBugsnagCode :: CodeIndex -> StackFrame -> StackFrame
attachBugsnagCode index sf = sf
    { stackFrame_code = findBugsnagCode
        (unpack $ stackFrame_file sf)
        (stackFrame_lineNumber sf)
        index
    }

findBugsnagCode :: FilePath -> Int -> CodeIndex -> Maybe (HashMap Int Text)
findBugsnagCode path n = fmap HashMap.fromList
    . findSourceRange path (begin, n + 3)
  where
    begin
        | n < 3 = 0
        | otherwise = n - 3

-- | Construct a 'StackFrame' from the point of this splice
--
-- Unfortunately there's no way to know the function, so that must be given:
--
currentStackFrame :: Q Exp
currentStackFrame = [|locStackFrame $(qLocation >>= liftLoc)|]

-- brittany-disable-next-binding

locStackFrame :: Loc -> Text -> StackFrame
locStackFrame (Loc path _ _ (ls, cs) _) func =
    defaultStackFrame
        { stackFrame_file = pack path
        , stackFrame_lineNumber = ls
        , stackFrame_columnNumber = Just cs
        , stackFrame_method = func
        , stackFrame_inProject = Just True
        -- N.B. this assumes we're unlikely to see adoption within libraries, or
        -- that such a thing would even work. If this function's used, it's
        -- assumed to be in end-user code.
        , stackFrame_code = Nothing
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
