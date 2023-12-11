{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- | Functions that throw
--
-- These are used in the test suite but are define here so that, hopefully, the
-- path/line/column will remain stable even if we re-organize the tests
-- themselves
module Examples where

import           Prelude

import           Control.Exception
import           Data.Bugsnag
import           GHC.Stack                  (HasCallStack)
import           Network.Bugsnag.Exception
import           Network.Bugsnag.StackFrame
import           UnliftIO.Exception         (throwString)

brokenFunctionIO :: IO a
brokenFunctionIO =
  throw $
    AsException $
      defaultException
        { exception_errorClass = "IOException"
        , exception_message = Just "Something exploded"
        , exception_stacktrace = [$(currentStackFrame) "brokenFunctionIO"]
        }

brokenFunction :: HasCallStack => a
brokenFunction = sillyHead [] `seq` undefined

sillyHead :: HasCallStack => [a] -> a
sillyHead (x : _) = x
sillyHead _       = error "empty list"

brokenFunction' :: HasCallStack => IO a
brokenFunction' = sillyHead' []

sillyHead' :: HasCallStack => [a] -> IO a
sillyHead' (x : _) = pure x
sillyHead' _       = throwString "empty list"

brokenFunction'' :: HasCallStack => IO a
brokenFunction'' = sillyHead'' []

sillyHead'' :: HasCallStack => [a] -> IO a
sillyHead'' (x : _) = pure x
sillyHead'' _       = throwString "empty list\n and message with newlines\n\n"
