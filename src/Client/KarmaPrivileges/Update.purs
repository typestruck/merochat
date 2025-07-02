module Client.KarmaPrivileges.Update where

import Prelude
import Shared.KarmaPrivileges.Types

import Data.Tuple.Nested ((/\))
import Flame (ListUpdate)
import Flame as F
import Shared.Modal.Types (ScreenModal(..))

update ∷ ListUpdate KarmaPrivilegesModel KarmaPrivilegesMessage
update model =
      case _ of
            ToggleVisibility modal → model { visible = modal == ShowKarmaPrivileges } /\ []
            ToggleBoardDisplay toggle → F.noMessages model
                  { toggleBoard = toggle
                  }