module Server.Login.Template where

import Prelude

import Effect (Effect)
import Flame.HTML.Attribute as HA
import Flame.HTML.Element as HE
import Flame.Renderer.String as FRS
import Server.Template (externalDefaultParameters)
import Server.Template as ST

template :: Effect String
template = do
        contents <- ST.template externalDefaultParameters {
                content = externalDefaultParameters.content <> content,
                javascript = javascript
        }
        FRS.render contents
        where   javascript = [
                        HE.script' [HA.type' "text/javascript", HA.src "/client/javascript/login.bundle.js"]
                ]
                content = [
                        HE.div (HA.class' "green-area green-box") [
                                HE.h2_ "Login to MelanChat",
                                HE.div (HA.class' "sign-up") [
                                        HE.input [HA.type' "text", HA.id "email", HA.placeholder "Email"],
                                        HE.input [HA.type' "password", HA.autocomplete false, HA.id "password", HA.placeholder "Password"],
                                        HE.input [HA.type' "button", HA.id "login", HA.value "Login"]
                                ],
                                HE.a [HA.href "/recover", HA.class' "question-link forgot"] "Forgot your password?",
                                HE.div [HA.class' "question-or"] [
                                        HE.hr' $ HA.class' "hr-or",
                                        HE.text "or",
                                        HE.hr' $ HA.class' "hr-or"
                                ],
                                HE.a [HA.href "/", HA.class' "question-link"] "Don't have an account?"

                        ]
                ]