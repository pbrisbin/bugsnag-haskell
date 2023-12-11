module Network.Bugsnag
  ( -- * Notifying
    notifyBugsnag
  , notifyBugsnagWith

    -- * Modifying events on notification
  , module Network.Bugsnag.BeforeNotify
  ) where

import           Network.Bugsnag.BeforeNotify
import           Network.Bugsnag.Notify
