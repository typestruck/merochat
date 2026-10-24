module Client.KarmaPrivileges.Update where

import Prelude

import Client.AppId (imAppId)
import Flame.Subscription as FS
import Shared.KarmaPrivileges.Types

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Effect.Class as EC
import Flame (Update)
import Shared.Im.Types as SIT
import Shared.Modal (Modal(..), ScreenModal(..))

update ∷ Update KarmaPrivilegesModel KarmaPrivilegesMessage
update model =
      case _ of
            ToggleVisibility modal → toggleVisibility model modal
            ToggleBoardDisplay toggle → model
                  { toggleBoard = toggle
                  } /\ []

toggleVisibility ∷ KarmaPrivilegesModel → Modal → KarmaPrivilegesModel /\ Array (Aff (Maybe KarmaPrivilegesMessage))
toggleVisibility model = case _ of
      HideModal → model /\ [ hide ]
      Screen modal → model { visible = modal == ShowKarmaPrivileges } /\ []
      _ → model /\ []
      where
      hide = EC.liftEffect do
            FS.send imAppId <<< SIT.SpecialRequest $ SIT.ToggleModal HideModal
            pure Nothing
