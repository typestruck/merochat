module Shared.IM.View where

import Prelude

import Effect (Effect)
import Flame (QuerySelector(..), Html)
import Flame as F
import Flame.HTML.Attribute as HA
import Flame.HTML.Element as HE
import Shared.Types

view :: IMModel -> Html IMMessage
view (IMModel model) = HE.div_ "IM"