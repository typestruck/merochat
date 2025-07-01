module Client.InternalBacker.Update where

import Prelude

import Data.Tuple.Nested ((/\))
import Flame (ListUpdate)
import Shared.Backer.Types (BackerMessage(..), BackerModel)
import Shared.Modal.Types (ScreenModal(..))

update ∷ ListUpdate BackerModel BackerMessage
update model =
      case _ of
            ToggleVisibility modal -> model { visible = modal == ShowBacker } /\ []
