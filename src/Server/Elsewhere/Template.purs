module Server.Elsewhere.Template where

import Prelude

import Effect (Effect)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Renderer.String as FRS
import Server.Template (externalDefaultParameters)
import Server.Template as ST
import Shared.Routes (routes)

template ∷ Effect String
template = do
      contents ← ST.template externalDefaultParameters
            { content = externalDefaultParameters.content <> content
            , title = "MeroChat - Logged out"
            }
      FRS.render contents
      where
      content =
            [ HE.div (HA.class' "green-area green-box")
                    [ HE.h2 (HA.class' "ext-heading") "Logged in from another device"
                    , HE.a (HA.href $ routes.login.get {}) "Click here to login again"
                    ]
            ]