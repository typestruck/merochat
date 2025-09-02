module Client.KarmaPrivileges.Main where

import Prelude

import Client.KarmaPrivileges.Update as CLU
import Effect (Effect)
import Web.DOM.ParentNode (QuerySelector(..))
import Flame as F
import Flame.Subscription as FS
import Shared.Im.EventTypes (modalVisible)
import Shared.KarmaPrivileges.Types (KarmaPrivilegesMessage(..))
import Shared.KarmaPrivileges.View as SLV
import Shared.Options.MountPoint (karmaPrivilegesId)

main ∷ Effect Unit
main = void $ F.resumeMount (QuerySelector "#karma-leaderboard") karmaPrivilegesId
      { view: SLV.view
      , subscribe: [ FS.onCustomEvent modalVisible ToggleVisibility ]
      , update: CLU.update
      }