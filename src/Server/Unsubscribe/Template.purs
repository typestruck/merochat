module Server.Unsubscribe.Template where

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
            , title = "MeroChat - Unsubscribe from Emails"
            }
      FRS.render contents
      where
      content =
            [ HE.div (HA.class' "green-area green-box")
                    [ HE.h2 (HA.class' "ext-heading") "Email unsubscribed"
                    , HE.p_ "You have been unsubscribed. If you wish to subscribe again, log in and modify your emails settings"
                    ]
            ]