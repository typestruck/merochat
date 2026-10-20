module Shared.Extra where

import Prelude

import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Types (NodeData)

br ∷ ∀ message. Html message
br = HE.text "\n"

fragment = HE.div_

a = HE.div

form = HE.div

managed a b c = HE.div' b

hr = HE.div'

select = HE.div

option = HE.div

link = HE.div'

script' = HE.div'

script = HE.div

meta = HE.div'

style = HE.div

-- | Placeholder until native rich-text rendering is implemented. Ignores the HTML.
innerHtml ∷ ∀ message. String → NodeData message
innerHtml _ = HA.createAttribute "data-inner-html-placeholder" ""
