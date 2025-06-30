module Client.KarmaPrivileges.Main where

import Prelude

import Client.KarmaPrivileges.Update as CLU
import Effect (Effect)
import Effect.Console as EC
import Flame (QuerySelector(..))
import Flame as F
import Shared.KarmaPrivileges.View as SLV
import Shared.Options.MountPoint (karmaPrivilegesId)

main ∷ Effect Unit
main = do
      EC.log "OIIIIIIIIIIIIIIIIIIIIIIIIIIII"
      F.resumeMount (QuerySelector ".karma-leaderboard") karmaPrivilegesId
            { view: SLV.view
            , subscribe: []
            , init: []
            , update: CLU.update
            }