module Client.InternalBacker.Main where

import Prelude

import Client.AppId (internalBackerAppId)
import Client.Lynx.Screen as CLS
import Client.InternalBacker.Update as CIBU
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as EA
import Effect.Class as EC
import Flame (Application)
import Flame as F
import Shared.Backer.Types (BackerMessage, BackerModel)
import Shared.Backer.View as SBV

main ∷ Effect Unit
main = EA.launchAff_ $ application >>= EC.liftEffect <<< F.mount internalBackerAppId

application ∷ Aff (Application BackerModel BackerMessage)
application = do
      let model = { visible: true }
      pure
            { model
            , view: SBV.view >>> pure >>> CLS.view internalBackerAppId
            , update: CIBU.update
            , subscribe: []
            }
