module Client.InternalHelp.Main where

import Prelude

import Client.AppId (internalHelpAppId)
import Client.Lynx.Screen as CLS
import Client.InternalHelp.Update as CIHU
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as EA
import Effect.Class as EC
import Flame (Application)
import Flame as F
import Shared.InternalHelp.Types (InternalHelpMessage, InternalHelpModel, DisplayHelpSection(..))
import Shared.InternalHelp.View as SIHV

main ∷ Effect Unit
main = EA.launchAff_ $ application >>= EC.liftEffect <<< F.mount internalHelpAppId

application ∷ Aff (Application InternalHelpModel InternalHelpMessage)
application = do
      let model = { visible: true, toggleHelp: FAQ }
      pure
            { model
            , view: SIHV.view >>> pure >>> CLS.view internalHelpAppId
            , update: CIHU.update
            , subscribe: []
            }
