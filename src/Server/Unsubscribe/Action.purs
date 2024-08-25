module Server.Unsubscribe.Action where

import Prelude

import Data.Maybe (Maybe(..))
import Server.Effect (ServerEffect)
import Server.Unsubscribe.Database as SUD

unsubscribe ∷ String → ServerEffect Boolean
unsubscribe token = do
      maybeId ← SUD.fetchUnsubscriber token
      case maybeId of
            Nothing → pure false
            Just id → do
                  SUD.unsubscribe id
                  pure true
