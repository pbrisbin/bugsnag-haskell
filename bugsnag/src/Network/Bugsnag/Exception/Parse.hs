-- |
--
-- Parse error messages for @'HasCallStack'@ information.
module Network.Bugsnag.Exception.Parse
  ( MessageWithStackFrames (..)
  , parseErrorCall
  , parseStringException
  ) where

import Prelude

import qualified Control.Exception as Exception
  ( ErrorCall
  , Exception
  , SomeException
  )
import Control.Monad (void)
import Data.Bifunctor (first)
import Data.Bugsnag
import Data.Text (Text, pack)
import Text.Parsec
import Text.Parsec.String

data MessageWithStackFrames = MessageWithStackFrames
  { mwsfMessage :: Text
  , mwsfStackFrames :: [StackFrame]
  }

-- | Parse an @'ErrorCall'@ for @'HasCallStack'@ information
parseErrorCall :: Exception.ErrorCall -> Either String MessageWithStackFrames
parseErrorCall = parse' errorCallParser

-- | Parse a @'StringException'@ for @'HasCallStack'@ information
--
-- We accept this as @'SomeException'@ so that this library doesn't depend on
-- any one concrete library that has @'throwString'@ (there are two right now,
-- sigh.)
parseStringException
  :: Exception.SomeException -> Either String MessageWithStackFrames
parseStringException = parse' stringExceptionParser

errorCallParser :: Parser MessageWithStackFrames
errorCallParser =
  MessageWithStackFrames
    <$> messageParser
    <*> manyTill stackFrameParser eof
 where
  messageParser :: Parser Text
  messageParser = do
    msg <- pack <$> manyTill anyChar eol
    msg <$ (string "CallStack (from HasCallStack):" *> eol)

  stackFrameParser :: Parser StackFrame
  stackFrameParser = do
    func <- stackFrameFunctionTill $ string ", called at "
    (path, ln, cl) <- stackFrameLocationTill $ eol <|> eof

    pure
      defaultStackFrame
        { stackFrame_file = pack path
        , stackFrame_lineNumber = ln
        , stackFrame_columnNumber = Just cl
        , stackFrame_method = func
        , stackFrame_inProject = Just True
        , stackFrame_code = Nothing
        }

stringExceptionParser :: Parser MessageWithStackFrames
stringExceptionParser =
  MessageWithStackFrames
    <$> messageParser
    <*> manyTill stackFrameParser eof
 where
  messageParser :: Parser Text
  messageParser = do
    manyTill anyChar (try $ string "throwString called with:") *> eol *> eol
    pack <$> manyTill anyChar (try $ eol *> string "Called from:" *> eol)

  stackFrameParser :: Parser StackFrame
  stackFrameParser = do
    func <- stackFrameFunctionTill $ string " ("
    (path, ln, cl) <- stackFrameLocationTill $ char ')' *> eol <|> eof

    pure
      defaultStackFrame
        { stackFrame_file = pack path
        , stackFrame_lineNumber = ln
        , stackFrame_columnNumber = Just cl
        , stackFrame_method = func
        , stackFrame_inProject = Just True
        , stackFrame_code = Nothing
        }

stackFrameFunctionTill :: Parser a -> Parser Text
stackFrameFunctionTill p = spaces *> (pack <$> manyTill anyChar p)

stackFrameLocationTill :: Parser a -> Parser (FilePath, Int, Int)
stackFrameLocationTill p = do
  result <-
    (,,)
      <$> manyTill anyChar (char ':')
      <*> (read <$> manyTill digit (char ':'))
      <*> (read <$> manyTill digit (char ' '))

  -- Ignore the "in package:module" part. TODO: we could use this to set
  -- bsfInProject if we had some more knowledge about project packages.
  void $ string "in "
  void $ manyTill anyChar $ char ':'
  void $ manyTill anyChar p
  pure result

parse'
  :: Exception.Exception e
  => Parser MessageWithStackFrames
  -> e
  -> Either String MessageWithStackFrames
parse' p = first show . parse (p <* eof) "<error>" . show

eol :: Parser ()
eol = void endOfLine
