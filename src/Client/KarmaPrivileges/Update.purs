module Client.KarmaPrivileges.Update where

import Prelude
import Shared.KarmaPrivileges.Types
import Flame as F
import Flame (ListUpdate)

update ∷ ListUpdate KarmaPrivilegesModel KarmaPrivilegesMessage
update model =
      case _ of
            ToggleBoardDisplay toggle → F.noMessages $ model
                  { toggleBoard = toggle
                  }