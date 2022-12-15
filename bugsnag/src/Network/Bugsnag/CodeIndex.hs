{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

-- | Compile-time snapshot of your project source
--
-- This is necessary to attach source code snippets to exceptions reported to
-- Bugsnag. We do this by reading the project source at compile-time and
-- stashing the result in 'Settings'.
--
-- **WARNING**: This feature (probably) means you will be holding all indexed
-- source code in memory during the life of your process. And in larger
-- projects, it will embed substantial amounts of source code in a single file,
-- which can significantly degrade compilation time.
--
module Network.Bugsnag.CodeIndex
    ( CodeIndex
    , buildCodeIndex
    , findSourceRange
    ) where

import Prelude

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Traversable (for)
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax
import System.FilePath.Glob (glob)

newtype CodeIndex = CodeIndex
    { unCodeIndex :: Map FilePath FileIndex
    }
    deriving stock (Lift, Show)

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
    { fiSourceLines :: Map Int Text
    , fiLastLine :: Int
    }
    deriving stock (Lift, Show)

buildFileIndex :: FilePath -> IO FileIndex
buildFileIndex path = do
    lns <- T.lines <$> T.readFile path

    pure FileIndex
        { fiSourceLines = Map.fromList $ zip [0 ..] lns
        , fiLastLine = length lns - 1
        }

findSourceRange :: FilePath -> (Int, Int) -> CodeIndex -> Maybe [(Int, Text)]
findSourceRange path (begin, end) index = do
    FileIndex {..} <- Map.lookup path $ unCodeIndex index

    for [begin .. min end fiLastLine]
        $ \n -> (n, ) <$> Map.lookup n fiSourceLines
