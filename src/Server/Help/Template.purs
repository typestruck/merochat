module Server.Help.Template where

import Prelude

import Effect (Effect)
import Environment (helpJSHash)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Renderer.String as FRS
import Server.Faq as SA
import Server.Privacy as SP
import Server.Template (externalDefaultParameters)
import Server.Template as ST
import Server.Terms as STM
import Shared.Path as SPT
import Shared.Types (ContentType(..))

template :: Effect String
template = do
      contents <- ST.template externalDefaultParameters {
            css = externalDefaultParameters.css <> [HE.link [HA.rel "stylesheet", HA.type' "text/css", HA.href $ SPT.pathery CSS "help.5c2d5d65952114e0b0e3"]],
            javascript = [HE.script' [HA.type' "text/javascript", HA.src <<< SPT.pathery JS $ "help." <> helpJSHash]],
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