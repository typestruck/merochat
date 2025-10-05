module Client.InternalBacker.Update where

import Prelude

import Data.Tuple.Nested ((/\))
import Debug (spy)
import Flame (Update)
import Shared.Backer.Types (BackerMessage(..), BackerModel)
import Shared.Modal.Types (ScreenModal(..))

update ∷ Update BackerModel BackerMessage
update model =
      case _ of
            ToggleVisibility modal → model { visible = modal == ShowBacker } /\ []
