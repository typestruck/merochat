module Shared.Im.Svg where

import Prelude
import Shared.Im.Types

import Data.Array ((:))
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Types (NodeData)

arrow ∷ Array (NodeData ImMessage) → Html ImMessage
arrow attrs = HE.svg (HA.viewBox "0 0 16 16" : attrs)
      [ HE.line' [ HA.strokeWidth "1.5px", HA.x1 "15.98", HA.y1 "8", HA.x2 "1.61", HA.y2 "8" ]
      , HE.polygon' [ HA.points "6.43 2.05 7.42 3.12 2.17 8 7.42 12.88 6.43 13.95 0.03 8 6.43 2.05" ]
      ]

home ∷ Html ImMessage
home = HE.svg [ HA.class' "svg-32 suggestions-home", HA.viewBox "0 0 24 24", HA.onClick ResumeSuggesting ]
      [ HE.path' [ HA.fillRule "evenodd", HA.clipRule "evenodd", HA.d "M5.27446 10.1262C5 10.7229 5 11.4018 5 12.7595V16.9999C5 18.8856 5 19.8284 5.58579 20.4142C6.11733 20.9457 6.94285 20.9949 8.5 20.9995V16C8.5 14.8954 9.39543 14 10.5 14H13.5C14.6046 14 15.5 14.8954 15.5 16V20.9995C17.0572 20.9949 17.8827 20.9457 18.4142 20.4142C19 19.8284 19 18.8856 19 16.9999V12.7595C19 11.4018 19 10.7229 18.7255 10.1262C18.4511 9.52943 17.9356 9.08763 16.9047 8.20401L15.9047 7.34687C14.0414 5.74974 13.1098 4.95117 12 4.95117C10.8902 4.95117 9.95857 5.74974 8.09525 7.34687L7.09525 8.20401C6.06437 9.08763 5.54892 9.52943 5.27446 10.1262ZM13.5 20.9999V16H10.5V20.9999H13.5Z" ]
      ]

contextMenu ∷ String → Html ImMessage
contextMenu id = HE.svg [ HA.id id, HA.class' "svg-32", HA.viewBox "0 0 16 16" ] contextMenuElements

contextMenuElements ∷ Array (Html ImMessage)
contextMenuElements =
      [ HE.circle' [ HA.class' "strokeless", HA.cx "8.03", HA.cy "4.04", HA.r "1" ]
      , HE.circle' [ HA.class' "strokeless", HA.cx "8.03", HA.cy "8.04", HA.r "1" ]
      , HE.circle' [ HA.class' "strokeless", HA.cx "8.04", HA.cy "12.04", HA.r "1" ]
      ]

closeElements ∷ ∀ message. Array (Html message)
closeElements =
      [ HE.path' [ HA.class' "strokeless", HA.d "M8,0a8,8,0,1,0,8,8A8,8,0,0,0,8,0ZM8,15.5A7.5,7.5,0,1,1,15.5,8,7.5,7.5,0,0,1,8,15.5Z" ]
      , HE.path' [ HA.class' "strokeless", HA.d "M11,5a.47.47,0,0,0-.35-.15h0a.47.47,0,0,0-.35.15L8,7.3,5.73,5a.49.49,0,0,0-.7.7L7.3,8,5,10.28A.48.48,0,0,0,5,11a.48.48,0,0,0,.7,0L8,8.7,10.27,11a.45.45,0,0,0,.35.15A.52.52,0,0,0,11,11a.5.5,0,0,0,0-.7L8.7,8,11,5.72A.48.48,0,0,0,11,5Z" ]
      ]

backArrow ∷ Html ImMessage
backArrow = HE.svg [ HA.class' "svg-55", HA.viewBox "0 0 16 16" ]
      [ HE.circle' [ HA.class' "strokeless light-circle", HA.cx "8", HA.cy "8", HA.r "8" ]
      , HE.polygon' [ HA.class' "fillless strokeless red-polygon", HA.points "4.88 7.99 9.37 3.5 10.29 4.42 6.73 7.99 10.32 11.58 9.39 12.5 5.81 8.91 5.8 8.91 4.88 7.99" ]
      ]

nextArrow ∷ Html ImMessage
nextArrow = HE.svg [ HA.class' "svg-55", HA.viewBox "0 0 16 16" ]
      [ HE.circle' [ HA.class' "strokeless light-circle", HA.cx "8", HA.cy "8", HA.r "8" ]
      , HE.polygon' [ HA.class' "fillless strokeless red-polygon", HA.points "11.02 7.99 6.53 3.5 5.61 4.42 9.17 7.99 5.58 11.58 6.5 12.5 10.09 8.91 10.1 8.91 11.02 7.99" ]
      ]

closeX ∷ Html ImMessage
closeX = HE.svg [ HA.class' "svg-55", HA.viewBox "0 0 48 48", HA.fill "none" ]
      [ HE.path' [ HA.d "M36 12L12 36", HA.strokeWidth "2", HA.strokeLinecap "round", HA.strokeLinejoin "round" ]
      , HE.path' [ HA.d "M12 12L36 36", HA.strokeWidth "2", HA.strokeLinecap "round", HA.strokeLinejoin "round" ]
      ]