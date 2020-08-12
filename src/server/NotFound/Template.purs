module Server.NotFound.Template where

import Prelude

import Effect (Effect)
import Flame.HTML.Element as HE
import Flame.Renderer.String as FRS
import Server.Template (externalDefaultParameters)
import Server.Template as ST
import Flame.HTML.Attribute as HA

template :: Effect String
template = do
      contents <- ST.template externalDefaultParameters { content = externalDefaultParameters.content <> content }
      FRS.render contents
      where content = [
                  HE.div (HA.class' "green-box") [
                        HE.h2_ "404! Not found!"
                  ]
            ]