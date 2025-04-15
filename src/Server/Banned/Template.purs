module Server.Banned.Template where

import Prelude

import Effect (Effect)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Renderer.String as FRS
import Server.Template (externalDefaultParameters)
import Server.Template as ST

template ∷ Effect String
template = do
      contents ← ST.template externalDefaultParameters
            { content = externalDefaultParameters.content <> content
            , title = "MeroChat - Temporarily banned"
            }
      FRS.render contents
      where
      content =
            [ HE.div (HA.class' "pastel-area")
                    [ HE.h2 (HA.class' "ext-heading") "Temporarily banned"
                    , HE.p_ "You have been temporarily banned from MeroChat. Remember, MeroChat is not"
                    , HE.div_
                            [ HE.div_ " a dating site"
                            , HE.div_ " a place to spread hate speech"
                            , HE.div_ " a marketplace"
                            ]
                    , HE.p_
                            [ HE.text "To restore your account, please reach us at "
                            , HE.a (HA.href "mailto:contact@mero.chat") "contact@mero.chat"
                            , HE.text ", "
                            , HE.a [ HA.href "https://reddit.com/r/MeroChat", HA.target "_blank" ] "r/MeroChat"
                            , HE.text " or "
                            , HE.a [ HA.href "https://twitter.com/MeroChat", HA.target "_blank" ] "@MeroChat"
                            ]
                    ]
            ]