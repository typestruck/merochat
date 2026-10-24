module Server.Banned.Template where

import Prelude

import Flame as F

import Effect (Effect)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Server.Template (externalDefaultParameters)
import Server.Template as ST
import Shared.Extra as SE
import Shared.Html (Html(..))

-- import Flame.Renderer.String as FRS

template ∷ Effect Html
template = do
      contents ← ST.template externalDefaultParameters
            { content = externalDefaultParameters.content <> content
            , title = "MeroChat - Temporarily banned"
            }
      -- Html <$> FRS.render contents
      pure $ Html ""

content ∷ ∀ message. Array (F.Html message)
content =
      [ HE.div [ HA.class' "pastel-area" ]
              [ SE.h2 [ HA.class' "ext-heading" ] [ HE.text "Temporarily banned" ]
              , SE.p_ [ HE.text "You have been temporarily banned from MeroChat. Remember, MeroChat is not" ]
              , HE.div_
                      [ HE.div_ [ HE.text " a dating site" ]
                      , HE.div_ [ HE.text " a place to spread hate speech" ]
                      , HE.div_ [ HE.text " a marketplace" ]
                      ]
              , SE.p [ HA.class' "lynx-inline" ]
                      [ HE.text "To restore your account, please reach us at "
                      , SE.a [ HA.href "mailto:contact@mero.chat", SE.externalLink "mailto:contact@mero.chat" ] [ HE.text "contact@mero.chat" ]
                      , HE.text ", "
                      , SE.a [ HA.href "https://reddit.com/r/MeroChat", SE.externalLink "https://reddit.com/r/MeroChat", HA.target "_blank" ] [ HE.text "r/MeroChat" ]
                      , HE.text " or "
                      , SE.a [ HA.href "https://twitter.com/MeroChat", SE.externalLink "https://twitter.com/MeroChat", HA.target "_blank" ] [ HE.text "@MeroChat" ]
                      ]
              ]
      ]
