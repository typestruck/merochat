module Client.KarmaPrivileges.Main where

import Prelude

import Client.KarmaPrivileges.Update as CLU
import Effect (Effect)
import Flame (QuerySelector(..))
import Flame as F
import Shared.KarmaPrivileges.View as SLV
import Shared.Options.MountPoint (karmaPrivilegesId)

main ∷ Effect Unit
main =
      F.resumeMount (QuerySelector ".karma-leaderboard") karmaPrivilegesId
            { view: SLV.view
            , subscribe: []
            , init: []
            , update: CLU.update
            }