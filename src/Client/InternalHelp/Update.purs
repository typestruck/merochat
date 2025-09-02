module Client.InternalHelp.Update where

import Prelude

import Data.Tuple.Nested ((/\))
import Flame (Update)
import Flame as F
import Shared.InternalHelp.Types (InternalHelpMessage(..), InternalHelpModel)
import Shared.Modal.Types (ScreenModal(..))

update ∷ Update InternalHelpModel InternalHelpMessage
update model =
      case _ of
            ToggleVisibility modal → model { visible = modal == ShowHelp } /\ []
            ToggleHelpSection toggle → F.noMessages $ model
                  { toggleHelp = toggle
                  }