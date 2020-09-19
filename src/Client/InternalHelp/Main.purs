module Client.InternalHelp.Main where

import Prelude

import Client.InternalHelp.Update as CIHU
import Effect (Effect)
import Flame (QuerySelector(..))
import Flame as F
import Shared.InternalHelp.View as SIHV

main :: Effect Unit
main =
      F.resumeMount_ (QuerySelector ".internal-help") {
            view: SIHV.view,
            init: [],
            update: CIHU.update
      }