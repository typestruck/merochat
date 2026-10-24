module Client.Navigation where

import Prelude

import Client.AppId as CA
import Client.Platform (isWeb)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Flame as F
import Shared.External.Types (Page(..))
import Shared.Routes (routesSpec)

foreign import openExternal ∷ String → Effect Unit

foreign import scrollToAnchor ∷ String → Effect Unit

navigate ∷ Page → Effect Unit
navigate page
      | isWeb = openExternal $ case page of
              Landing → routesSpec.landing {}
              Login → routesSpec.login.get {}
              Help → routesSpec.help {} <> "#faq"
              Terms → routesSpec.help {} <> "#terms"
              Privacy → routesSpec.help {} <> "#privacy"
              Backer → routesSpec.backer {}
              Recover → routesSpec.recover.get { query: { token: Nothing } }
              Banned → "/banned"
      | otherwise = case page of
              Landing → F.navigate CA.landingAppId
              Login → F.navigate CA.loginAppId
              Help → F.navigate CA.helpAppId
              Terms → F.navigate CA.termsAppId
              Privacy → F.navigate CA.privacyAppId
              Backer → F.navigate CA.backerAppId
              Recover → F.navigate CA.recoverAppId
              Banned → F.navigate CA.bannedAppId
