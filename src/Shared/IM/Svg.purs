module Shared.IM.Svg where

import Data.Array ((:))
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Types (NodeData)
import Shared.Types (IMMessage)

arrow :: Array (NodeData IMMessage) -> Html IMMessage
arrow attrs = HE.svg (HA.viewBox "0 0 16 16" : attrs) [
    HE.line' [HA.stroke "#cedf6d", HA.strokeWidth "1.5px", HA.x1 "15.98", HA.y1 "8", HA.x2 "1.61", HA.y2 "8"],
    HE.polygon' [HA.points "6.43 2.05 7.42 3.12 2.17 8 7.42 12.88 6.43 13.95 0.03 8 6.43 2.05"]
]

contextMenu :: String -> Html IMMessage
contextMenu id = HE.svg [HA.id id, HA.class' "svg-32", HA.viewBox "0 0 16 16"] contextMenuElements

contextMenuElements :: Array (Html IMMessage)
contextMenuElements = [
    HE.circle' [HA.class' "strokeless", HA.cx "8.03", HA.cy "4.04", HA.r "1"],
    HE.circle' [HA.class' "strokeless", HA.cx "8.03", HA.cy "8.04", HA.r "1"],
    HE.circle' [HA.class' "strokeless", HA.cx "8.04", HA.cy "12.04", HA.r "1"]
]
