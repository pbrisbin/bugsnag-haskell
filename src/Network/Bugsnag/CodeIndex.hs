{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Network.Bugsnag.CodeIndex
    ( CodeIndex
    , buildCodeIndex
    , findSourceRange
    ) where

import Data.List (genericLength)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Traversable (for)
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax
import Numeric.Natural (Natural)
import System.FilePath.Glob (glob)

newtype CodeIndex = CodeIndex
    { unCodeIndex :: Map FilePath FileIndex }
    deriving (Lift, Show)

buildCodeIndex :: String -> Q Exp
buildCodeIndex p = do
    index <- qRunIO $ buildCodeIndex' p
    [|index|]

buildCodeIndex' :: String -> IO CodeIndex
buildCodeIndex' p = do
    paths <- glob p
    CodeIndex . Map.fromList <$> traverse indexPath paths
  where
    indexPath :: FilePath -> IO (FilePath, FileIndex)
    indexPath fp = (fp, ) <$> buildFileIndex fp

data FileIndex = FileIndex
    { fiSourceLines :: Map Natural Text
    , fiLastLine :: Natural
    }
    deriving (Lift, Show)

buildFileIndex :: FilePath -> IO FileIndex
buildFileIndex path = do
    lns <- T.lines <$> T.readFile path

    pure FileIndex
        { fiSourceLines = Map.fromList $ zip [0 ..] lns
        , fiLastLine = genericLength lns - 1
        }

findSourceRange
    :: FilePath -> (Natural, Natural) -> CodeIndex -> Maybe [(Natural, Text)]
findSourceRange path (begin, end) index = do
    FileIndex {..} <- Map.lookup path $ unCodeIndex index

    for [begin .. min end fiLastLine]
        $ \n -> (n, ) <$> Map.lookup n fiSourceLines
