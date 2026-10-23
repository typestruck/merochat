module Client.Lynx.Main where

import Prelude

import Client.AppId (imAppId, landingAppId, loginAppId, profileAppId)
import Client.Im.Main as CIM
import Client.Login.Main as CLM
import Client.Profile.Main as CPM
import Client.Lynx.Room as CLR
import Client.Lynx.Session as CLS
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as EA
import Effect.Class as EC
import Flame (Application, Html)
import Flame as F
import Flame.Html.Element as HE
import Server.Landing.Template (content)
import Shared.User (User)

main ∷ Effect Unit
main = EA.launchAff_ do
      authenticated ← EC.liftEffect do
            CLS.initialize
            CLS.isAuthenticated
      when authenticated do
            user ← CLR.select ∷ Aff (Maybe User)
            case user of
                  Nothing → EC.liftEffect CLS.clear
                  Just _ → pure unit
      EC.liftEffect do
            F.mountRoutes
                  [ F.makeGuardedRoute loginAppId (pure $ CLM.application CLM.authenticateNative) $ redirectLogged imAppId
                  , F.makeGuardedRoute imAppId CIM.application <<< redirectAnonymous $ loginAppId
                  , F.makeGuardedRoute profileAppId CPM.application <<< redirectAnonymous $ loginAppId
                  , F.makeGuardedRoute landingAppId (pure $ other content) $ redirectLogged imAppId
                  ]

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
      , view: const $ HE.div_ content
      , update: \model _ → F.noMessages model
      , subscribe: []
      }
