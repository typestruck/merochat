module Client.Lynx.Main where

import Prelude

import Client.AppId (imAppId, landingAppId, loginAppId, profileAppId, helpAppId, termsAppId, privacyAppId, backerAppId, recoverAppId, bannedAppId)
import Client.Backer.Main as CBAM
import Client.Banned.Main as CBNM
import Client.Help.Main as CHM
import Client.Im.Main as CIM
import Client.Landing.Main as CLAM
import Client.Login.Main as CLM
import Client.Lynx.Room as CLR
import Client.Lynx.Session as CLS
import Client.Privacy.Main as CPRM
import Client.Profile.Main as CPM
import Client.Recover.Main as CRM
import Client.Terms.Main as CTM
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as EA
import Effect.Class as EC
import Flame as F
import Flame.Subscription as FS
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
            F.mountRoutes $
                  [ F.makeGuardedRoute loginAppId (pure CLM.nativeApplication) $ redirectLogged imAppId
                  , F.makeGuardedRoute imAppId CIM.application (redirectAnonymous loginAppId)
                  , F.makeGuardedRoute profileAppId CPM.application (redirectAnonymous loginAppId)
                  , F.makeGuardedRoute landingAppId (pure CLAM.application) $ redirectLogged imAppId
                  , F.makeGuardedRoute helpAppId (pure CHM.application) $ redirectLogged imAppId
                  , F.makeGuardedRoute termsAppId (pure CTM.application) $ redirectLogged imAppId
                  , F.makeGuardedRoute privacyAppId (pure CPRM.application) $ redirectLogged imAppId
                  , F.makeGuardedRoute backerAppId (pure CBAM.application) $ redirectLogged imAppId
                  , F.makeGuardedRoute recoverAppId (pure CRM.application) $ redirectLogged imAppId
                  , F.makeGuardedRoute bannedAppId (pure CBNM.application) $ redirectLogged imAppId
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
