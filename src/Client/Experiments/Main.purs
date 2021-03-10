module Client.Experiments.Main where

import Prelude

import Client.Experiments.Update as CEU
import Effect (Effect)
import Flame (QuerySelector(..))
import Flame as F
import Shared.Experiments.View as SEV

main :: Effect Unit
main =
      F.resumeMount_ (QuerySelector ".chat-experiments") {
            view: SEV.view,
            init: [],
            update: CEU.update
      }