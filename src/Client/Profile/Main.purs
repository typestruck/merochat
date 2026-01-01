module Client.Profile.Main where

import Prelude

import Client.AppId (profileAppId)
import Client.File as CCF
import Client.Profile.Update as CPU
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Flame as F
import Flame.Subscription as FS
import Shared.Element (ElementId(..))
import Shared.Element as SE
import Client.EventTypes (modalVisible)
import Shared.Profile.Types (ProfileMessage(..))
import Shared.Profile.View as SPV

main ∷ Effect Unit
main = do
      void $ F.resumeMount (SE.toQuerySelector ProfileEditionForm) profileAppId
            { view: SPV.view
            , subscribe: [ FS.onCustomEvent modalVisible ToggleVisibility ]
            , update: CPU.update
            }
