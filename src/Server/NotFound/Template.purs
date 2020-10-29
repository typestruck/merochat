module Server.NotFound.Template where

import Prelude

import Effect (Effect)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Renderer.String as FRS
import Server.Template (externalDefaultParameters)
import Server.Template as ST

template :: Effect String
template = do
      contents <- ST.template externalDefaultParameters { content = content }
      FRS.render contents
      where content = [
                  HE.div (HA.class' "green-box") [
                        HE.div (HA.class' "error-page") [
                              HE.div (HA.class' "error-page-code") "404",
                              HE.div_ "The requested page could not be found"

                        ]
                  ]
            ]