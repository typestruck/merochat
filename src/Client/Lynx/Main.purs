module Client.Lynx.Main where

import Prelude

import Client.AppId (imAppId, landingAppId, loginAppId)
import Client.Im.Main as CIM
import Client.Login.Main as CLM
import Client.Lynx.Session as CLS
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Flame (Application, Html)
import Flame as F
import Flame.Html.Element as HE
import Server.Landing.Template (content)

main ∷ Effect Unit
main = do
      CLS.initialize
      F.mountRoutes
            [ F.makeGuardedRoute loginAppId (pure $ CLM.application CLM.authenticateNative) $ redirectLogged imAppId
            , F.makeGuardedRoute imAppId CIM.application <<< redirectAnonymous $ loginAppId
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
