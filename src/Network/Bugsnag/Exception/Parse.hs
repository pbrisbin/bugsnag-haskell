{-# LANGUAGE OverloadedStrings #-}
-- |
--
-- Parse error messages for @'HasCallStack'@ information.
--
module Network.Bugsnag.Exception.Parse
    ( MessageWithStackFrames(..)
    , parseErrorCall
    , parseStringException
    ) where

import Control.Exception (ErrorCall, Exception, SomeException)
import Control.Monad (void)
import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as T
import Network.Bugsnag.StackFrame
import Numeric.Natural
import Text.Parsec
import Text.Parsec.String

data MessageWithStackFrames = MessageWithStackFrames
    { mwsfMessage :: Text
    , mwsfStackFrames :: [BugsnagStackFrame]
    }

-- | Parse an @'ErrorCall'@ for @'HasCallStack'@ information
parseErrorCall :: ErrorCall -> Either String MessageWithStackFrames
parseErrorCall = parse' errorCallParser

-- | Parse a @'StringException'@ for @'HasCallStack'@ information
--
-- We accept this as @'SomeException'@ so that this library doesn't depend on
-- any one concrete library that has @'throwString'@ (there are two right now,
-- sigh.)
--
parseStringException :: SomeException -> Either String MessageWithStackFrames
parseStringException = parse' stringExceptionParser

-- brittany-disable-next-binding

errorCallParser :: Parser MessageWithStackFrames
errorCallParser = MessageWithStackFrames
    <$> messageParser
    <*> manyTill stackFrameParser eof
  where
    messageParser :: Parser Text
    messageParser = do
        msg <- T.pack <$> manyTill anyChar eol
        msg <$ (string "CallStack (from HasCallStack):" *> eol)

    stackFrameParser :: Parser BugsnagStackFrame
    stackFrameParser = do
        func <- stackFrameFunctionTill $ string ", called at "
        (path, ln, cl) <- stackFrameLocationTill $ eol <|> eof

        pure BugsnagStackFrame
            { bsfFile = path
            , bsfLineNumber = ln
            , bsfColumnNumber = Just cl
            , bsfMethod = func
            , bsfInProject = Just True
            , bsfCode = Nothing
            }

-- brittany-disable-next-binding

stringExceptionParser :: Parser MessageWithStackFrames
stringExceptionParser = MessageWithStackFrames
    <$> messageParser
    <*> manyTill stackFrameParser eof
  where
    messageParser :: Parser Text
    messageParser = do
        manyTill anyChar (try $ string "throwString called with:") *> eol *> eol
        msg <- T.pack <$> manyTill anyChar eol
        msg <$ (string "Called from:" *> eol)

    stackFrameParser :: Parser BugsnagStackFrame
    stackFrameParser = do
        func <- stackFrameFunctionTill $ string " ("
        (path, ln, cl) <- stackFrameLocationTill $ char ')' *> eol <|> eof

        pure BugsnagStackFrame
            { bsfFile = path
            , bsfLineNumber = ln
            , bsfColumnNumber = Just cl
            , bsfMethod = func
            , bsfInProject = Just True
            , bsfCode = Nothing
            }

stackFrameFunctionTill :: Parser a -> Parser Text
stackFrameFunctionTill p = spaces *> (T.pack <$> manyTill anyChar p)

stackFrameLocationTill :: Parser a -> Parser (FilePath, Natural, Natural)
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
    :: Exception e
    => Parser MessageWithStackFrames
    -> e
    -> Either String MessageWithStackFrames
parse' p = first show . parse (p <* eof) "<error>" . show

eol :: Parser ()
eol = void endOfLine
