module Shared.Extra where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Flame.Html.Event as FHE
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Types (NodeData)

br ∷ ∀ message. Html message
br = HE.div' [ HA.class' "line-break" ]

fragment = HE.div_

a ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
a = HE.div

onClick ∷ ∀ message. Effect Unit → NodeData message
onClick action = FHE.createRawEvent "bindtap" $ const $ action $> Nothing

foreign import openExternal ∷ String → Effect Unit

foreign import scrollToAnchor ∷ String → Effect Unit

externalLink ∷ ∀ message. String → NodeData message
externalLink = onClick <<< openExternal

anchor ∷ ∀ message. String → NodeData message
anchor = onClick <<< scrollToAnchor

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

-- | A selectable row using Lynx text and tap events.
choice ∷ ∀ message. Boolean → message → String → Html message
choice selected message label = HE.createElement "text"
      [ HA.class' { "lynx-choice": true, "lynx-choice-selected": selected }
      , HA.onClick message
      ]
      [ HE.text $ (if selected then "● " else "○ ") <> label ]

p ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
p attributes children = HE.div [ HA.class' "public-paragraph" ] [ HE.div attributes children ]

p_ ∷ ∀ message. Array (Html message) → Html message
p_ = p []

h1 ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
h1 attributes children = HE.div [ HA.class' "public-title" ] [ HE.div attributes children ]

h1_ ∷ ∀ message. Array (Html message) → Html message
h1_ = h1 []

h2 ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
h2 attributes children = HE.div [ HA.class' "public-heading" ] [ HE.div attributes children ]

h2_ ∷ ∀ message. Array (Html message) → Html message
h2_ = h2 []

h3 ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
h3 attributes children = HE.div [ HA.class' "public-subheading" ] [ HE.div attributes children ]

h3_ ∷ ∀ message. Array (Html message) → Html message
h3_ = h3 []

ul ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
ul attributes children = HE.div [ HA.class' "public-list" ] [ HE.div attributes children ]

ul_ ∷ ∀ message. Array (Html message) → Html message
ul_ = ul []

ol ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
ol attributes children = HE.div [ HA.class' "public-list" ] [ HE.div attributes children ]

ol_ ∷ ∀ message. Array (Html message) → Html message
ol_ = ol []

li ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
li attributes children = HE.div [ HA.class' "public-list-item" ] [ HE.div attributes children ]

li_ ∷ ∀ message. Array (Html message) → Html message
li_ = li []
