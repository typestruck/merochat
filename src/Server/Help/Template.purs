module Server.Help.Template where

import Prelude

import Effect (Effect)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Server.Template (externalDefaultParameters)
import Server.Template as ST
import Shared.Extra as SE
import Shared.Faq (faq)
import Shared.Html (Html(..))
import Shared.Privacy (privacy)
import Shared.Resource (Bundle(..), ResourceType(..))
import Shared.Resource as SR
import Shared.Terms (terms)

-- import Flame.Renderer.String as FRS

template ∷ Effect Html
template = do
      contents ← ST.template externalDefaultParameters
            { css = externalDefaultParameters.css <> [ SE.link [ HA.rel "stylesheet", HA.type' "text/css", HA.href $ SR.bundlePath Help Css ] ]
            , javascript = [ SE.script' [ HA.type' "text/javascript", HA.src $ SR.bundlePath Help Js ] ]
            , content = externalDefaultParameters.content <> content
            , title = "MeroChat - Help"
            }
      -- Html <$> FRS.render contents
      pure $ Html ""
      where
      content =
            [ HE.div [ HA.class' "external-help" ]
                    [ HE.div [ HA.class' "modal-menu" ]
                            [ HE.div [ HA.class' "entry selected", HA.id "faq-link" ] [ HE.text "FAQ" ]
                            , HE.div [ HA.class' "entry", HA.id "terms-link" ] [ HE.text "Terms and conditions" ]
                            , HE.div [ HA.class' "entry", HA.id "privacy-link" ] [ HE.text "Privacy policy" ]
                            ]
                    , HE.div [ HA.class' "help" ]
                            [ HE.div [ HA.id "faq" ] [ faq ]
                            , HE.div [ HA.id "terms", HA.class' "hidden" ] [ terms ]
                            , HE.div [ HA.id "privacy", HA.class' "hidden" ] [ privacy ]
                            ]
                    ]
            ]
