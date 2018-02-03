module Network.Bugsnag.Throw
    ( throwBugsnag
    ) where

import Control.Exception (throwIO)
import Data.Text (Text)
import Network.Bugsnag.Exception
import Network.Bugsnag.StackFrame

-- | Convenience function for throwing a @'BugsnagException'@
throwBugsnag
    :: Text -- ^ Error class
    -> Text -- ^ Message
    -> Text -- ^ Function name
    -> (Text -> BugsnagStackFrame)
    -- ^ How to construct the current stack frame, given function name
    --
    -- This should always be the @$('currentStackFrame')@ splice. It must be
    -- specified at the point of throw so the location is correct.
    --
    -> IO a
throwBugsnag errorClass message func toStackFrame = throwIO
    (bugsnagException errorClass [toStackFrame func])
    { beMessage = Just message }
