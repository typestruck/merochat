module Client.Experiments.Main where

import Prelude

import Client.Experiments.Update as CEU
import Effect (Effect)
import Flame (QuerySelector(..))
import Flame as F
import Shared.Experiments.View as SEV
import Shared.Options.MountPoint (MountPoint(..), experimentsId)

main ∷ Effect Unit
main =
      F.resumeMount (QuerySelector ".chat-experiments") experimentsId
            { view: SEV.view
            , subscribe: []
            , init: []
            , update: CEU.update
            }