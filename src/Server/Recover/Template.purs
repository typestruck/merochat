module Server.Recover.Template where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Flame.HTML.Attribute as HA
import Flame.HTML.Element as HE
import Flame.Renderer.String as FRS
import Server.Template (externalDefaultParameters, externalFooter)
import Server.Template as ST

template :: Maybe String -> Effect String
template token = do
      contents <- ST.template externalDefaultParameters {
            content = externalDefaultParameters.content <> content,
            javascript = javascript
      }
      FRS.render contents
      where javascript = [
                  HE.script' [HA.type' "text/javascript", HA.src "/client/javascript/recover.bundle.js"],
                  --we need a global callback for grecaptha so we need to directly call the bundled code
                  HE.script (HA.type' "text/javascript") "window.completeRecover = function(cpt){return Recover.completeRecover(cpt)();}; Recover.main();",
                  HE.script' $ HA.src "https://www.google.com/recaptcha/api.js"
            ]
            content = [
                  HE.div (HA.class' "green-area green-box") [
                              HE.h2 (HA.class' "ext-heading") "Recover password",
                              case token of
                                    Nothing -> HE.div (HA.class' "form-up") [
                                          HE.input [HA.type' "text", HA.id "email", HA.placeholder "Email"],
                                          HE.input [HA.type' "button", HA.id "login", HA.value "Recover"]
                                    ]
                                    Just t ->
                                          HE.div (HA.class' "form-up") [
                                                HE.div_ [
                                                      HE.label_ "Password",
                                                      HE.input [HA.type' "password"]
                                                ],
                                                HE.div_ [
                                                      HE.label_ "Confirm password",
                                                      HE.input [HA.type' "password"]
                                                ],
                                                HE.input [HA.type' "button", HA.value "Change password", HA.class' "action-button"]
                                          ],
                              HE.a [HA.href "/login", HA.class' "question-link forgot"] "Already have an account?",
                              HE.div [HA.class' "question-or"] [
                                    HE.hr' $ HA.class' "hr-or",
                                    HE.text "or",
                                    HE.hr' $ HA.class' "hr-or"
                              ],
                              HE.a [HA.href "/", HA.class' "question-link"] "Don't have an account?"

                  ]
            ]