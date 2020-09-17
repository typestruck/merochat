module Server.Leaderboard.Template where

import Prelude

import Effect (Effect)
import Flame.HTML.Attribute as HA
import Flame.HTML.Element as HE
import Flame.Renderer.String as FRS
import Server.Template (defaultParameters)
import Server.Template as ST

template :: Effect String
template = do
      contents <- ST.template defaultParameters {
            content = content
      }
      FRS.render contents
      where content = [
            HE.h1_ "Leaderboard"
      ]