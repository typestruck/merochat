module Client.KarmaPrivileges.Main where

import Prelude

import Client.KarmaPrivileges.Update as CLU
import Effect (Effect)
import Flame (QuerySelector(..))
import Flame as F
import Flame.Subscription as FS
import Shared.Im.EventTypes (modalVisible)
import Shared.KarmaPrivileges.Types (KarmaPrivilegesMessage(..))
import Shared.KarmaPrivileges.View as SLV
import Shared.Options.MountPoint (karmaPrivilegesId)

main ∷ Effect Unit
main = F.resumeMount (QuerySelector "#karma-leaderboard") karmaPrivilegesId
      { view: SLV.view
      , subscribe: [ FS.onCustomEvent modalVisible ToggleVisibility ]
      , init: []
      , update: CLU.update
      }