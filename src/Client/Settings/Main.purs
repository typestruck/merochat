module Client.Settings.Main where

import Prelude

import Client.AppId (settingsAppId)
import Client.EventTypes (modalVisible)
import Client.Settings.Update as CSU
import Effect (Effect)
import Flame as F
import Flame.Subscription as FS
import Shared.Settings.Types (SettingsMessage(..))
import Shared.Settings.View as SSV
import Web.DOM.ParentNode (QuerySelector(..))

main ∷ Effect Unit
main =
      void $ F.resumeMount (QuerySelector "#settings-edition") settingsAppId
            { view: SSV.view
            , subscribe: [ FS.onCustomEvent modalVisible ToggleVisibility ]
            , update: CSU.update
            }