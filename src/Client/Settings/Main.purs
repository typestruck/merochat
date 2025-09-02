module Client.Settings.Main where

import Prelude

import Client.Settings.Account as CSA
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Flame as F
import Flame.Subscription as FS
import Shared.Im.EventTypes (modalVisible)
import Shared.Settings.Types (SettingsMessage(..))
import Shared.Settings.View as SSV
import Web.DOM.ParentNode (QuerySelector(..))

main ∷ Effect Unit
main =
      void $ F.resumeMount_ (QuerySelector "#settings-edition")
            { view: SSV.view
            , subscribe: [ FS.onCustomEvent modalVisible ToggleVisibility ]
            , update: CSA.update
            }