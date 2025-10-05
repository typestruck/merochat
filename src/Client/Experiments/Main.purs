module Client.Experiments.Main where

import Prelude

import Client.Experiments.Update as CEU
import Effect (Effect)
import Web.DOM.ParentNode (QuerySelector(..))
import Flame as F
import Flame.Subscription as FS
import Shared.Experiments.Types (ExperimentsMessage(..))
import Shared.Experiments.View as SEV
import Shared.Im.EventTypes (modalVisible)
import Shared.Options.MountPoint (experimentsId)

main ∷ Effect Unit
main =
      void $ F.resumeMount (QuerySelector "#chat-experiments") experimentsId
            { view: SEV.view
            , subscribe: [ FS.onCustomEvent modalVisible ToggleVisibility ]
            , update: CEU.update
            }