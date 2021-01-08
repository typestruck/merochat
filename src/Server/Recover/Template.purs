module Server.Recover.Template where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Renderer.String as FRS
import Server.Template (externalDefaultParameters)
import Server.Template as ST
import Shared.Options.Profile (emailMaxCharacters, passwordMaxCharacters, passwordMinCharacters)
import Shared.Path as SP
import Shared.Routes (routes)
import Shared.Types (ContentType(..))

template :: Maybe String -> Effect String
template token = do
      contents <- ST.template externalDefaultParameters {
            content = externalDefaultParameters.content <> content,
            javascript = javascript
      }
      FRS.render contents
      where javascript = [
                  HE.script' [HA.type' "text/javascript", HA.src $ SP.pathery JS "recover.1a5fa8368956a8294e65"],
                  HE.script' $ HA.src "https://www.google.com/recaptcha/api.js"
            ]
            content = [
                  HE.div (HA.class' "green-area green-box") [
                              case token of
                                    Nothing ->
                                          HE.fragment [
                                                HE.h2 (HA.class' "ext-heading") "Recover account",
                                                HE.div (HA.class' "form-up") [
                                                      HE.div [HA.id "email-input", HA.class' "input"] [
                                                            HE.label_ "Email",
                                                            HE.input [HA.type' "text", HA.id "email", HA.maxlength emailMaxCharacters],
                                                            HE.span (HA.class' "error-message") "Please enter a valid email"
                                                      ],
                                                      HE.div [HA.class' "input"][
                                                            HE.input [HA.type' "button", HA.value "Recover"]
                                                      ],
                                                      HE.span' [HA.class' "request-error-message error-message"],
                                                      HE.span [HA.id "request-success-message", HA.class' "success-message"] "Recovery email sent. Please check your inbox." ,
                                                      HE.div' [HA.class' "g-recaptcha", HA.createAttribute "data-sitekey" "6LeDyE4UAAAAABhlkiT86xpghyJqiHfXdGZGJkB0", HA.id "captcha", HA.createAttribute "data-callback" "completeRecover", HA.createAttribute "data-size" "invisible"]
                                                ]
                                          ]
                                    Just t ->
                                          HE.fragment [
                                                HE.h2 (HA.class' "ext-heading") "Reset password",
                                                HE.div (HA.class' "form-up") [
                                                      HE.div [HA.id "password-input", HA.class' "input"] [
                                                            HE.label_ "Password",
                                                            HE.input [HA.type' "password", HA.maxlength passwordMaxCharacters, HA.id "password"],
                                                            HE.span (HA.class' "error-message") $ "Password must be " <> show passwordMinCharacters <> " characters or more"
                                                      ],
                                                      HE.div [HA.id "confirm-password-input", HA.class' "input"] [
                                                            HE.label_ "Confirm password",
                                                            HE.input [HA.type' "password", HA.maxlength passwordMaxCharacters, HA.id "confirm-password"],
                                                            HE.span (HA.class' "error-message") "Password and confirmation do not match"
                                                      ],
                                                      HE.div [HA.class' "input"] [
                                                            HE.input [HA.type' "button", HA.value "Change password", HA.class' "action-button"]
                                                      ],
                                                      HE.span' [HA.class' "request-error-message error-message"],
                                                      HE.span [HA.class' "success-message"] $ "Password reseted. Redirecting to login..."
                                                ]
                                          ],
                              HE.a [HA.href $ routes.login.get {}, HA.class' "question-link forgot"] "Already have an account?",
                              HE.div [HA.class' "question-or"] [
                                    HE.hr' $ HA.class' "hr-or",
                                    HE.text "or",
                                    HE.hr' $ HA.class' "hr-or"
                              ],
                              HE.a [HA.href $ routes.landing {}, HA.class' "question-link"] "Don't have an account?"
                  ]
            ]