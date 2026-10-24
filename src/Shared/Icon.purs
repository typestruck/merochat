module Shared.Icon where

import Prelude

import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Types (NodeData)

svg ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
svg attributes = HE.svg $ [ HA.fill "currentColor", HA.stroke "currentColor" ] <> attributes

outline ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
outline attributes = HE.svg $ [ HA.fill "none", HA.stroke "currentColor" ] <> attributes

illustration ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
illustration attributes = HE.svg $ [ HA.fill "none", HA.stroke "none" ] <> attributes

filled ∷ ∀ message. Array (NodeData message) → Array (Html message) → Html message
filled attributes = HE.svg $ [ HA.fill "currentColor", HA.stroke "none" ] <> attributes
