-- | Working with Bugsnag's 'event_metaData' field
module Network.Bugsnag.MetaData
  ( MetaData (..)
  , metaData
  ) where

import Prelude

import Data.Aeson.Compat ((.=), Value(Object), Object, object)
import qualified Data.Aeson.Compat as Aeson

newtype MetaData = MetaData
  { unMetaData :: Object
  }
  deriving stock (Eq, Show)

instance Semigroup MetaData where
  -- \| /Right/-biased, recursive union
  --
  -- The chosen bias ensures that adding metadata in smaller scopes (later)
  -- overrides values from larger scopes.
  MetaData x <> MetaData y = MetaData $ unionObjects y x
   where
    unionObjects :: Object -> Object -> Object
    unionObjects = Aeson.unionWith unionValues

    unionValues (Object a) (Object b) = Object $ unionObjects a b
    unionValues a _ = a

instance Monoid MetaData where
  mempty = MetaData mempty

-- | Construct 'MetaData' from 'Pair's
metaData
  :: Aeson.Key
  -- ^ The Tab within which the values will display
  -> [Aeson.Pair]
  -- ^ The Key-Values themselves
  -> MetaData
metaData key = MetaData . Aeson.fromList . pure . (key .=) . object

-- $details
--
-- From <https://bugsnagerrorreportingapi.docs.apiary.io/#reference/0/notify/send-error-reports>
--
-- @events[].metaData@
--
-- > An object containing any further data you wish to attach to this error
-- > event. This should contain one or more objects, with each object being
-- > displayed in its own tab on the event details on Bugsnag.
-- >
-- > {
-- >     // Custom user data to be displayed in the User tab along with standard
-- >     // user fields on the Bugsnag website.
-- >     "user": {
-- >        ...
-- >     },
-- >
-- >     // Custom app data to be displayed in the App tab along with standard
-- >     // app fields on the Bugsnag website.
-- >     "app": {
-- >        ...
-- >     },
-- >
-- >     // Custom device data to be displayed in the Device tab along with
-- >     //standard device fields on the Bugsnag website.
-- >     "device": {
-- >        ...
-- >     },
-- >
-- >     Custom request data to be displayed in the Request tab along with
-- >     standard request fields on the Bugsnag website.
-- >     "request": {
-- >        ...
-- >     },
-- >
-- >     // This will be displayed as an extra tab on the Bugsnag website.
-- >     "Some data": {
-- >
-- >         // A key value pair that will be displayed in the first tab.
-- >         "key": "value",
-- >
-- >         // Key value pairs can be contained in nested objects which helps
-- >         // to organise the information presented in the tab.
-- >         "setOfKeys": {
-- >             "key": "value",
-- >             "key2": "value"
-- >         }
-- >     },
-- >
-- >     // This would be the second extra tab on the Bugsnag website.
-- >     "Some more data": {
-- >         ...
-- >     }
-- > }
