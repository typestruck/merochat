module Server.InternalError.Template where

import Prelude

import Effect (Effect)
import Flame.HTML.Attribute as HA
import Flame.HTML.Element as HE
import Flame.Renderer.String as FRS
import Server.Template (externalDefaultParameters)
import Server.Template as ST

template :: String -> Effect String
template reason = do
      contents <- ST.template externalDefaultParameters { content = externalDefaultParameters.content <> content }
      FRS.render contents
      where content = [
                  HE.div (HA.class' "green-box") [
                        HE.h2_ "500... An error occurred. We are looking into that!",
                        HE.span_ reason
                  ]
            ]