module Client.InternalHelp.Update where

import Prelude
import Shared.Types

import Flame as F
import Flame (ListUpdate)

update :: ListUpdate InternalHelpModel InternalHelpMessage
update model =
      case _ of
            ToggleHelpSection toggle -> F.noMessages $ model {
                toggleHelp = toggle
            }