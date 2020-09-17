module Client.Leaderboard.Main where

import Prelude

import Client.Leaderboard.Update as CLU
import Effect (Effect)
import Flame (QuerySelector(..))
import Flame as F
import Shared.Leaderboard.View as SLV

main :: Effect Unit
main =
      F.resumeMount_ (QuerySelector ".karma-leaderboard") {
            view: SLV.view,
            init: [],
            update: CLU.update
      }