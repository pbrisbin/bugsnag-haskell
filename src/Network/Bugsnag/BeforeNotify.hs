module Network.Bugsnag.BeforeNotify
    ( BeforeNotify
    , beforeNotify
    , runBeforeNotify

    -- * Modifying the underlying Exceptions
    , updateExceptions
    , filterExceptions
    , updateStackFrames
    , filterStackFrames
    , setStackFramesCode
    , setStackFramesInProject
    , setStackFramesInProjectByFile
    , setStackFramesInProjectBy

    -- * Modifying the Event
    , updateEvent
    , updateEventFromOriginalException
    , updateEventFromWaiRequest
    , updateEventFromWaiRequestUnredacted
    , redactRequestHeaders
    , setGroupingHash
    , setGroupingHashBy
    , setDevice
    , setRequest
    , setWarningSeverity
    , setErrorSeverity
    , setInfoSeverity
    ) where

import Prelude

import qualified Control.Exception as Exception
import Data.Bugsnag
import Data.Maybe (isJust)
import Data.Text (Text, unpack)
import Network.Bugsnag.BugsnagRequestHeaders
import Network.Bugsnag.CodeIndex
import Network.Bugsnag.Device
import Network.Bugsnag.Request
import Network.Bugsnag.StackFrame
import Network.HTTP.Types.Header (HeaderName)
import qualified Network.Wai as Wai (Request)

-- $setup
-- >>> import qualified Data.HashMap.Strict as HashMap
-- >>> import Data.Maybe (fromMaybe)

-- | A function from 'Event' to 'Event' that is applied before notifying
--
-- The wrapped function also accepts the original exception, for cases in which
-- that's useful -- but it's often not. Most 'BeforeNotify's use 'updateEvent',
-- which discards it.
--
-- 'BeforeNotify' implements 'Semigroup' and 'Monoid', which means the /no
-- nothing/ 'BeforeNotify' is 'mempty' and two 'BeforeNotify's @doThis@ then
-- @doThat@ can be implemented as @doThat <> doThis@.
--
newtype BeforeNotify = BeforeNotify
    { _unBeforeNotify :: forall e. Exception.Exception e => e -> Event -> Event
    }

instance Semigroup BeforeNotify where
    BeforeNotify f <> BeforeNotify g = BeforeNotify $ \e -> f e . g e

instance Monoid BeforeNotify where
    mempty = BeforeNotify $ const id

beforeNotify
    :: (forall e . Exception.Exception e => e -> Event -> Event)
    -> BeforeNotify
beforeNotify = BeforeNotify

runBeforeNotify :: Exception.Exception e => BeforeNotify -> e -> Event -> Event
runBeforeNotify (BeforeNotify f) = f

updateExceptions :: (Exception -> Exception) -> BeforeNotify
updateExceptions f = updateEvent
    $ \event -> event { event_exceptions = map f $ event_exceptions event }

filterExceptions :: (Exception -> Bool) -> BeforeNotify
filterExceptions p = updateEvent $ \event ->
    event { event_exceptions = filter p $ event_exceptions event }

updateStackFrames :: (StackFrame -> StackFrame) -> BeforeNotify
updateStackFrames f = updateExceptions
    $ \e -> e { exception_stacktrace = map f $ exception_stacktrace e }

filterStackFrames :: (StackFrame -> Bool) -> BeforeNotify
filterStackFrames p = updateExceptions
    $ \e -> e { exception_stacktrace = filter p $ exception_stacktrace e }

setStackFramesCode :: CodeIndex -> BeforeNotify
setStackFramesCode =
    (setStackFramesInProjectBy (isJust . stackFrame_code) <>)
        . updateStackFrames
        . attachBugsnagCode

setStackFramesInProject :: Bool -> BeforeNotify
setStackFramesInProject = setStackFramesInProjectBy . const

setStackFramesInProjectByFile :: (FilePath -> Bool) -> BeforeNotify
setStackFramesInProjectByFile f =
    setStackFramesInProjectBy $ f . unpack . stackFrame_file

setStackFramesInProjectBy :: (StackFrame -> Bool) -> BeforeNotify
setStackFramesInProjectBy f =
    updateStackFrames $ \sf -> sf { stackFrame_inProject = Just $ f sf }

updateEvent :: (Event -> Event) -> BeforeNotify
updateEvent f = beforeNotify $ \_e event -> f event

-- | Update the 'Event' based on the original exception
--
-- This allows updating the Event after casting to an exception type that this
-- library doesn't know about (e.g. @SqlError@). Because the result of your
-- function is itself a 'BeforeNotify', you can (and should) use other
-- helpers:
--
-- @
-- myBeforeNotify =
--     'defaultBeforeNotify'
--         <> 'updateEventFromOriginalException' asSqlError
--         <> 'updateEventFromOriginalException' asHttpError
--         <> -- ...
--
-- asSqlError :: SqlError -> BeforeNotify
-- asSqlError SqlError{..} =
--     'setGroupingHash' sqlErrorCode <> 'updateException' (\e -> e
--         { exception_errorClass = sqlErrorCode
--         , exception_message = Just sqlErrorMessage
--         })
-- @
--
-- If the cast fails, the event is unchanged.
--
updateEventFromOriginalException
    :: forall e . Exception.Exception e => (e -> BeforeNotify) -> BeforeNotify
updateEventFromOriginalException f = beforeNotify $ \e event ->
    let bn = maybe mempty f $ Exception.fromException $ Exception.toException e
    in runBeforeNotify bn e event

-- | Set the events 'Event' and 'Device'
--
-- This function redacts the following Request headers:
--
-- - Authorization
-- - Cookie
-- - X-XSRF-TOKEN (CSRF token header used by Yesod)
--
-- To avoid this, use 'updateEventFromWaiRequestUnredacted'.
--
updateEventFromWaiRequest :: Wai.Request -> BeforeNotify
updateEventFromWaiRequest wrequest =
    redactRequestHeaders ["Authorization", "Cookie", "X-XSRF-TOKEN"]
        <> updateEventFromWaiRequestUnredacted wrequest

updateEventFromWaiRequestUnredacted :: Wai.Request -> BeforeNotify
updateEventFromWaiRequestUnredacted wrequest =
    let
        mdevice = bugsnagDeviceFromWaiRequest wrequest
        request = bugsnagRequestFromWaiRequest wrequest
    in maybe mempty setDevice mdevice <> setRequest request

-- | Redact the given request headers
--
-- Headers like @Authorization@ may contain information you don't want to report
-- to Bugsnag.
--
-- > redactRequestHeaders ["Authorization", "Cookie"]
--
redactRequestHeaders :: [HeaderName] -> BeforeNotify
redactRequestHeaders headers = updateEvent $ \event ->
    event { event_request = redactHeaders headers <$> event_request event }

-- |
--
-- >>> let headers = [("Authorization", "secret"), ("X-Foo", "Bar")]
-- >>> let req = defaultRequest { request_headers = Just $ HashMap.fromList headers }
-- >>> fmap HashMap.toList $ request_headers $ redactHeaders ["Authorization"] req
-- Just [("Authorization","<redacted>"),("X-Foo","Bar")]
--
redactHeaders :: [HeaderName] -> Request -> Request
redactHeaders headers request = request
    { request_headers = redactBugsnagRequestHeaders headers
        <$> request_headers request
    }

setGroupingHash :: Text -> BeforeNotify
setGroupingHash hash = setGroupingHashBy $ const $ Just hash

setGroupingHashBy :: (Event -> Maybe Text) -> BeforeNotify
setGroupingHashBy f =
    updateEvent $ \event -> event { event_groupingHash = f event }

-- | Set the Event's Request
--
-- See 'bugsnagRequestFromWaiRequest'
--
setRequest :: Request -> BeforeNotify
setRequest request =
    updateEvent $ \event -> event { event_request = Just request }

-- | Set the Event's Device
--
-- See 'bugsnagDeviceFromWaiRequest'
--
setDevice :: Device -> BeforeNotify
setDevice device = updateEvent $ \event -> event { event_device = Just device }

-- | Set to 'ErrorSeverity'
setErrorSeverity :: BeforeNotify
setErrorSeverity = setSeverity errorSeverity

-- | Set to 'WarningSeverity'
setWarningSeverity :: BeforeNotify
setWarningSeverity = setSeverity warningSeverity

-- | Set to 'InfoSeverity'
setInfoSeverity :: BeforeNotify
setInfoSeverity = setSeverity infoSeverity

setSeverity :: Severity -> BeforeNotify
setSeverity severity =
    updateEvent $ \event -> event { event_severity = Just severity }
