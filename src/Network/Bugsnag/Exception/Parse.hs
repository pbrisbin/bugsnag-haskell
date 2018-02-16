{-# LANGUAGE OverloadedStrings #-}
-- |
--
-- Parse error messages for @'HasCallStack'@ information.
--
module Network.Bugsnag.Exception.Parse
    ( MessageWithStackFrames(..)
    , parseErrorCall
    ) where

import Control.Exception (ErrorCall)
import Control.Monad (void)
import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as T
import Network.Bugsnag.StackFrame
import Text.Parsec
import Text.Parsec.String

data MessageWithStackFrames = MessageWithStackFrames
    { mwsfMessage :: Text
    , mwsfStackFrames :: [BugsnagStackFrame]
    }

-- | Parse an @'ErrorCall'@ for @'HasCallStack'@ information
parseErrorCall :: ErrorCall -> Either String MessageWithStackFrames
parseErrorCall = first show . parse (errorCallParser <* eof) "<error>" . show

errorCallParser :: Parser MessageWithStackFrames
errorCallParser = MessageWithStackFrames
    <$> (T.pack <$> manyTill anyChar eol)
    <*> (csHeader *> manyTill stackFrameParser eof)

csHeader :: Parser ()
csHeader = string "CallStack (from HasCallStack):" *> eol

stackFrameParser :: Parser BugsnagStackFrame
stackFrameParser = do
    func <- spaces *> (T.pack <$> manyTill anyChar (char ','))
    path <- string " called at " *> manyTill anyChar (char ':')
    ln <- read <$> manyTill digit (char ':')
    cl <- read <$> manyTill digit (char ' ')

    void $ string "in " -- package:module
    void $ manyTill anyChar $ char ':'
    void $ manyTill anyChar $ eol <|> eof

    pure BugsnagStackFrame
        { bsfFile = path
        , bsfLineNumber = ln
        , bsfColumnNumber = Just cl
        , bsfMethod = func
        , bsfInProject = Just True
        , bsfCode = Nothing
        }

eol :: Parser ()
eol = void endOfLine
