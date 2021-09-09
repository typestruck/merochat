module Client.InternalHelp.Update where

import Prelude

import Flame (ListUpdate)
import Flame as F
import Shared.InternalHelp.Types (InternalHelpMessage(..), InternalHelpModel)

update ∷ ListUpdate InternalHelpModel InternalHelpMessage
update model =
      case _ of
            ToggleHelpSection toggle → F.noMessages $ model
                  { toggleHelp = toggle
                  }