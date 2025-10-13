module Client.InternalBacker.Main where

import Prelude

import Client.InternalBacker.Update as CIHU
import Effect (Effect)
import Web.DOM.ParentNode (QuerySelector(..))
import Flame as F
import Flame.Subscription as FS
import Client.EventTypes (modalVisible)
import Shared.Backer.Types (BackerMessage(..))
import Shared.Backer.View as SIHV

main ∷ Effect Unit
main =
      void $ F.resumeMount_ (QuerySelector "#backer")
            { view: SIHV.view
            , subscribe: [ FS.onCustomEvent modalVisible ToggleVisibility ]
            , update: CIHU.update
            }