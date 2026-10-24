module Client.KarmaPrivileges.Main where

import Prelude

import Client.AppId (karmaPrivilegesAppId)
import Client.Lynx.Screen as CLS
import Client.KarmaPrivileges.Update as CKPU
import Client.Network (routes)
import Client.Network as CN
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as EA
import Effect.Class as EC
import Flame (Application)
import Flame as F
import Record as R
import Shared.KarmaPrivileges.Types (KarmaPrivilegesMessage, KarmaPrivilegesModel, ToggleBoard(..))
import Shared.KarmaPrivileges.View as SKPV

main ∷ Effect Unit
main = EA.launchAff_ $ application >>= EC.liftEffect <<< F.mount karmaPrivilegesAppId

application ∷ Aff (Application KarmaPrivilegesModel KarmaPrivilegesMessage)
application = do
      payload ← CN.silentRequest $ routes.karmaInitialize {}
      let model = R.merge { visible: true, toggleBoard: InBetween10 } payload
      pure
            { model
            , view: SKPV.view >>> pure >>> CLS.view karmaPrivilegesAppId
            , update: CKPU.update
            , subscribe: []
            }
