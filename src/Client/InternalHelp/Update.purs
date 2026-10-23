module Client.InternalHelp.Update where

import Prelude

import Client.AppId (imAppId)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Effect.Class as EC
import Flame (Update)
import Flame as F
import Flame.Subscription as FS
import Shared.InternalHelp.Types (InternalHelpMessage(..), InternalHelpModel)
import Shared.Im.Types as SIT
import Shared.Modal (Modal(..), ScreenModal(..))

update ∷ Update InternalHelpModel InternalHelpMessage
update model =
      case _ of
            ToggleVisibility modal → toggleVisibility model modal
            ToggleHelpSection toggle → F.noMessages $ model
                  { toggleHelp = toggle
                  }

toggleVisibility ∷ InternalHelpModel → Modal → InternalHelpModel /\ Array (Aff (Maybe InternalHelpMessage))
toggleVisibility model = case _ of
      HideModal → model /\ [ hide ]
      Screen modal → model { visible = modal == ShowHelp } /\ []
      _ → model /\ []
      where
      hide = EC.liftEffect do
            FS.send imAppId <<< SIT.SpecialRequest $ SIT.ToggleModal HideModal
            pure Nothing
