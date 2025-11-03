module Server.Banned.Template where

import Prelude

import Effect (Effect)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Renderer.String as FRS
import Server.Template (externalDefaultParameters)
import Server.Template as ST
import Shared.Html (Html(..))

template ∷ Effect Html
template = do
      contents ← ST.template externalDefaultParameters
            { content = externalDefaultParameters.content <> content
            , title = "MeroChat - Temporarily banned"
            }
      Html <$> FRS.render contents
      where
      content =
            [ HE.div [ HA.class' "pastel-area" ]
                    [ HE.h2 [ HA.class' "ext-heading" ] [ HE.text "Temporarily banned" ]
                    , HE.p_ [ HE.text "You have been temporarily banned from MeroChat. Remember, MeroChat is not" ]
                    , HE.div_
                            [ HE.div_ [ HE.text " a dating site" ]
                            , HE.div_ [ HE.text " a place to spread hate speech" ]
                            , HE.div_ [ HE.text " a marketplace" ]
                            ]
                    , HE.p_
                            [ HE.text "To restore your account, please reach us at "
                            , HE.a [ HA.href "mailto:contact@mero.chat" ] [ HE.text "contact@mero.chat" ]
                            , HE.text ", "
                            , HE.a [ HA.href "https://reddit.com/r/MeroChat", HA.target "_blank" ] [ HE.text "r/MeroChat" ]
                            , HE.text " or "
                            , HE.a [ HA.href "https://twitter.com/MeroChat", HA.target "_blank" ] [ HE.text "@MeroChat" ]
                            ]
                    ]
            ]