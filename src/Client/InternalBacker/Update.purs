module Client.InternalBacker.Update where

import Prelude

import Client.AppId (imAppId)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Debug (spy)
import Effect.Aff (Aff)
import Effect.Class as EC
import Flame (Update)
import Flame.Subscription as FS
import Shared.Backer.Types (BackerMessage(..), BackerModel)
import Shared.Im.Types as SIT
import Shared.Modal (Modal(..), ScreenModal(..))

update ∷ Update BackerModel BackerMessage
update model =
      case _ of
            ToggleVisibility modal → toggleVisibility model modal

toggleVisibility ∷ BackerModel → Modal → BackerModel /\ Array (Aff (Maybe BackerMessage))
toggleVisibility model = case _ of
      HideModal → model /\ [ hide ]
      Screen modal → model { visible = modal == ShowBacker } /\ []
      _ → model /\ []
      where
      hide = EC.liftEffect do
            FS.send imAppId <<< SIT.SpecialRequest $ SIT.ToggleModal HideModal
            pure Nothing
