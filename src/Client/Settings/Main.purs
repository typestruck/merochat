module Client.Settings.Main where

import Prelude

import Client.Settings.Account as CSA
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Flame.Application.Effectful as FAE
import Flame.Subscription as FS
import Shared.Im.EventTypes (modalVisible)
import Shared.Settings.Types (SettingsMessage(..))
import Shared.Settings.View as SSV
import Web.DOM.ParentNode (QuerySelector(..))

main ∷ Effect Unit
main =
      FAE.resumeMount_ (QuerySelector "#settings-edition")
            { view: SSV.view
            , subscribe: [ FS.onCustomEvent modalVisible ToggleVisibility ]
            , init: Nothing
            , update: CSA.update
            }