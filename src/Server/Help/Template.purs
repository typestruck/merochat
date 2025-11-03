module Server.Help.Template where

import Prelude

import Effect (Effect)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Renderer.String as FRS
import Server.Faq as SA
import Server.Privacy as SP
import Server.Template (externalDefaultParameters)
import Server.Template as ST
import Server.Terms as STM
import Shared.Html (Html(..))
import Shared.Resource (Bundle(..), ResourceType(..))
import Shared.Resource as SPT

template ∷ Effect Html
template = do
      contents ← ST.template externalDefaultParameters
            { css = externalDefaultParameters.css <> [ HE.link [ HA.rel "stylesheet", HA.type' "text/css", HA.href $ SPT.bundlePath Help Css ] ]
            , javascript = [ HE.script' [ HA.type' "text/javascript", HA.src $ SPT.bundlePath Help Js ] ]
            , content = externalDefaultParameters.content <> content
            , title = "MeroChat - Help"
            }
      Html <$> FRS.render contents
      where
      content =
            [ HE.div [ HA.class' "external-help" ]
                    [ HE.div [ HA.class' "modal-menu" ]
                            [ HE.div [ HA.class' "entry selected", HA.id "faq-link" ] [ HE.text "FAQ" ]
                            , HE.div [ HA.class' "entry", HA.id "terms-link" ] [ HE.text "Terms and conditions" ]
                            , HE.div [ HA.class' "entry", HA.id "privacy-link" ] [ HE.text "Privacy policy" ]
                            ]
                    , HE.div [ HA.class' "help" ]
                            [ HE.div [ HA.id "faq" ] [ SA.faq ]
                            , HE.div [ HA.id "terms", HA.class' "hidden" ] [ STM.terms ]
                            , HE.div [ HA.id "privacy", HA.class' "hidden" ] [ SP.privacy ]
                            ]
                    ]
            ]