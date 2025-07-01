module Client.InternalHelp.Main where

import Prelude

import Client.InternalHelp.Update as CIHU
import Effect (Effect)
import Flame (QuerySelector(..))
import Flame as F
import Flame.Subscription as FS
import Shared.Im.EventTypes (modalVisible)
import Shared.InternalHelp.Types (InternalHelpMessage(..))
import Shared.InternalHelp.View as SIHV

main ∷ Effect Unit
main =
      F.resumeMount_ (QuerySelector "#internal-help")
            { view: SIHV.view
            , subscribe: [ FS.onCustomEvent modalVisible ToggleVisibility]
            , init: []
            , update: CIHU.update
            }