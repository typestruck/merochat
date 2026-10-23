module Client.Lynx.Main where

import Prelude

import Client.AppId (imAppId, landingAppId, loginAppId, profileAppId)
import Client.Im.Main as CIM
import Client.Login.Main as CLM
import Client.Lynx.Room as CLR
import Client.Lynx.Session as CLS
import Client.Profile.Main as CPM
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as EA
import Effect.Class as EC
import Flame (Application, Html)
import Flame as F
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Subscription as FS
import Server.Landing.Template (content)
import Shared.Im.Types (ImMessage(..))
import Shared.Theme as ST
import Shared.User (User)

main ∷ Effect Unit
main = EA.launchAff_ do
      EC.liftEffect ST.initialize
      authenticated ← EC.liftEffect do
            CLS.initialize
            CLS.isAuthenticated
      when authenticated do
            user ← CLR.select ∷ Aff (Maybe User)
            when (DM.isNothing user) $ EC.liftEffect CLS.clear
      EC.liftEffect do
            F.mountRoutes
                  [ F.makeGuardedRoute loginAppId (pure $ CLM.nativeApplication) $ redirectLogged imAppId
                  , F.makeGuardedRoute imAppId CIM.application <<< redirectAnonymous $ loginAppId
                  , F.makeGuardedRoute profileAppId CPM.application <<< redirectAnonymous $ loginAppId
                  , F.makeGuardedRoute landingAppId (pure $ other content) $ redirectLogged imAppId
                  ]

            FS.send imAppId ConnectWebSocket
            CLS.onSessionEnded $ F.navigate loginAppId
      where
      redirectLogged destination = do
            authenticated ← CLS.isAuthenticated
            pure $ if authenticated then Just destination else Nothing

      redirectAnonymous destination = do
            authenticated ← CLS.isAuthenticated
            pure $ if authenticated then Nothing else Just destination

other ∷ Array (Html Unit) → Application Unit Unit
other content =
      { model: unit
      , view: const $ HE.div [ HA.id "landing", ST.event "landing" "bindlayoutchange" ] content
      , update: \model _ → F.noMessages model
      , subscribe: []
      }
