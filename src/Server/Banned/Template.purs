module Server.Banned.Template where

import Prelude

import Effect (Effect)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Renderer.String as FRS
import Server.Template (defaultParameters)
import Server.Template as ST

template ∷ Effect String
template = do
      contents ← ST.template defaultParameters
            { content = content
            , css = css
            , title = "MeroChat - Logged out"
            }
      FRS.render contents
      where
      content =
            [ HE.div (HA.class' "green-area green-box")
                    [ HE.h2 (HA.class' "ext-heading") "You logged in from another device" ]
            ]
      css = []