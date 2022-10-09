module Client.KarmaPrivileges.Main where

import Prelude

import Client.KarmaPrivileges.Update as CLU
import Effect (Effect)
import Flame (QuerySelector(..))
import Flame as F
import Shared.KarmaPrivileges.View as SLV

main ∷ Effect Unit
main =
      F.resumeMount_ (QuerySelector ".karma-leaderboard")
            { view: SLV.view
            , subscribe: []
            , init: []
            , update: CLU.update
            }