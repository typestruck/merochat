module Server.Login.Template where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Flame.HTML.Attribute as HA
import Flame.HTML.Element as HE
import Flame.Renderer.String as FRS
import Server.Template (externalDefaultParameters)
import Server.Template as ST
import Shared.Options.Profile (passwordMinCharacters)
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
                              HE.div [HA.id "email-input", HA.class' "input"] [
                                    HE.label_ "Email",
                                    HE.input [HA.type' "text", HA.id "email"],
                                    HE.span (HA.class' "error-message") "Please enter a valid email"
                              ],
                              HE.div [HA.id "password-input", HA.class' "input"] [
                                    HE.label_ "Password",
                                    HE.input [HA.type' "password", HA.id "password"],
                                    HE.span (HA.class' "error-message") $ "Password must be " <> show passwordMinCharacters <> " characters or more"
                              ],
                              HE.input [HA.type' "button", HA.value "Log in"],
                              HE.span' [HA.class' "request-error-message error-message"]
                        ],
                        HE.a [HA.href $ routes.recover.get {query: {token: Nothing}}, HA.class' "question-link forgot"] "Forgot your password?",
                        HE.div [HA.class' "question-or"] [
                              HE.hr' $ HA.class' "hr-or",
                              HE.text "or",
                              HE.hr' $ HA.class' "hr-or"
                        ],
                        HE.a [HA.href $ routes.landing {}, HA.class' "question-link"] "Don't have an account?"
                  ]
            ]