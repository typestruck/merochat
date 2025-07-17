module Client.InternalBacker.Main where

import Prelude

import Client.InternalBacker.Update as CIHU
import Effect (Effect)
import Flame (QuerySelector(..))
import Flame as F
import Flame.Subscription as FS
import Shared.Im.EventTypes (modalVisible)
import Shared.Backer.Types (BackerMessage(..))
import Shared.Backer.View as SIHV

main ∷ Effect Unit
main =
      F.resumeMount_ (QuerySelector "#backer")
            { view: SIHV.view
            , subscribe: [ FS.onCustomEvent modalVisible ToggleVisibility ]
            , init: []
            , update: CIHU.update
            }