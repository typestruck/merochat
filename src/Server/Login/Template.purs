module Server.Login.Template where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Flame.HTML.Attribute as HA
import Flame.HTML.Element as HE
import Flame.Renderer.String as FRS
import Server.Template (externalDefaultParameters)
import Server.Template as ST
import Shared.Routes (routes)


template :: Effect String
template = do
      contents <- ST.template externalDefaultParameters {
            content = externalDefaultParameters.content <> content,
            javascript = javascript
      }
      FRS.render contents
      where javascript = [
                  HE.script' [HA.type' "text/javascript", HA.src "/client/javascript/login.bundle.js"]
            ]
            content = [
                  HE.div (HA.class' "green-area green-box") [
                        HE.h2 (HA.class' "ext-heading") "Login to MelanChat",
                        HE.div (HA.class' "form-up") [
                              HE.input [HA.type' "text", HA.id "email", HA.placeholder "Email"],
                              HE.input [HA.type' "password", HA.id "password", HA.placeholder "Password"],
                              HE.input [HA.type' "button", HA.id "login", HA.value "Login"]
                        ],
                        HE.a [HA.href $ routes.recover.get {query: {token: Nothing}}, HA.class' "question-link forgot"] "Forgot your password?",
                        HE.div [HA.class' "question-or"] [
                              HE.hr' $ HA.class' "hr-or",
                              HE.text "or",
                              HE.hr' $ HA.class' "hr-or"
                        ],
                        HE.a [HA.href $ routes.landing.get {}, HA.class' "question-link"] "Don't have an account?"

                  ]
            ]