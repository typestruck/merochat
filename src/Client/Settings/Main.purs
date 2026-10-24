module Client.Settings.Main where

import Prelude

import Client.AppId (settingsAppId)
import Client.Lynx.Screen as CLS
import Client.Settings.Update as CSU
import Client.Network (routes)
import Client.Network as CN
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as EA
import Effect.Class as EC
import Flame (Application)
import Flame as F
import Record as R
import Shared.Settings.Types (SettingsMessage, SettingsModel, Tab(..))
import Shared.Settings.View as SSV

main ∷ Effect Unit
main = EA.launchAff_ $ application >>= EC.liftEffect <<< F.mount settingsAppId

application ∷ Aff (Application SettingsModel SettingsMessage)
application = do
      settings ← CN.silentRequest $ routes.settings.initialize {}
      let model = R.merge
            { email: ""
            , emailConfirmation: ""
            , password: ""
            , passwordConfirmation: ""
            , confirmTermination: false
            , visible: true
            , tab: Privacy
            , erroredFields: []
            , requestStatus: Nothing
            , accountRequest: Nothing
            , accountLoading: false
            } settings
      pure
            { model
            , view: SSV.view >>> pure >>> CLS.view settingsAppId
            , update: CSU.update
            , subscribe: []
            }
