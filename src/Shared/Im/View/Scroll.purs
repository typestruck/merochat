module Shared.Im.Scroll where

import Flame.Html.Event (Event)
import Flame.Html.Event as FHE
import Flame.Types (NodeData)
import Shared.Im.Types (ImMessage)

--safari does not support scrollEnd because it is a shitty browser
foreign import scrollEventName ∷ String

onScrollEvent ∷ (Event → ImMessage) → NodeData ImMessage
onScrollEvent = FHE.createEventMessage scrollEventName
