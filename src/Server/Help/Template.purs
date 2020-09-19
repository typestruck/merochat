module Server.Help.Template where

import Prelude

import Effect (Effect)
import Flame.HTML.Attribute as HA
import Flame.HTML.Element as HE
import Flame.Renderer.String as FRS
import Server.Faq as SA
import Server.Privacy as SP
import Server.Template (externalDefaultParameters)
import Server.Template as ST
import Server.Terms as STM

template :: Effect String
template = do
      contents <- ST.template externalDefaultParameters {
            css = externalDefaultParameters.css <> [HE.link [HA.rel "stylesheet", HA.type' "text/css", HA.href "/client/css/help.css"]],
            javascript = [HE.script' [HA.type' "text/javascript", HA.src "/client/javascript/help.bundle.js"] ],
            content = externalDefaultParameters.content <> content
      }
      FRS.render contents
      where content = [
                  HE.div (HA.class' "external-help") [
                        HE.div (HA.class' "modal-menu") [
                              HE.div [HA.class' "entry selected", HA.id "faq-link"] "FAQ",
                              HE.div [HA.class' "entry", HA.id "terms-link"] "Terms and conditions",
                              HE.div [HA.class' "entry", HA.id "privacy-link"] "Privacy policy"
                        ],
                        HE.div (HA.class' "help") [
                              HE.div "faq" SA.faq,
                              HE.div [HA.id "terms", HA.class' "hidden"] STM.terms,
                              HE.div [HA.id "privacy", HA.class' "hidden"] SP.privacy
                        ]
                  ]
            ]