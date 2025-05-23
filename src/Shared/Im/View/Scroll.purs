module Shared.Im.Scroll where

import Prelude

import Flame.Html.Event as HA
import Flame.Types (NodeData)
import Shared.Im.Types (ImMessage)

--safari does not support scrollEnd because it is a shitty browser
foreign import scrollEventName ∷ String

onScrollEvent :: ImMessage -> NodeData ImMessage
onScrollEvent = HA.createEvent scrollEventName