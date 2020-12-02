module Server.InternalError.Template where

import Prelude

import Effect (Effect)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Renderer.String as FRS
import Server.Template (externalDefaultParameters)
import Server.Template as ST

template :: String -> Effect String
template reason = do
      contents <- ST.template externalDefaultParameters { content = content }
      FRS.render contents
      where content = [
                  HE.div (HA.class' "green-box") [
                        HE.div (HA.class' "error-page") [
                              HE.div (HA.class' "error-page-code") "500",
                              HE.div_ "An internal error occurred. We will look into it.",
                              HE.div (HA.class' "reason") reason
                        ]
                  ]
            ]