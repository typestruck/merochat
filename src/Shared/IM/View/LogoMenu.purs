module Shared.IM.View.LogoMenu where

import Prelude
import Shared.Types

import Data.Maybe (Maybe)
import Data.Maybe as DM
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE


logoMenu :: Maybe String -> Html IMMessage
logoMenu fortune = HE.div (HA.class' "relative") [
      HE.div (HA.class' "tabs") [
            HE.div_ [
                 HE.text "Contacts"
            ],
            HE.div (HA.onClick ToggleInitialScreen) "Suggestions"
      ],
      HE.div (HA.class' {fortune: true, hidden: DM.isNothing fortune }) [
            HE.div (HA.class' "fortune-deets") [
                  HE.text $ DM.fromMaybe "" fortune
            ],
            HE.svg [HA.viewBox "0 0 512 512", HA.onClick (ToggleFortune false) ] [
                  HE.title "Close",
                  HE.polygon' $ HA.points "438.627 118.627 393.373 73.373 256 210.746 118.627 73.373 73.373 118.627 210.746 256 73.373 393.373 118.627 438.627 256 301.254 393.373 438.627 438.627 393.373 301.254 256 438.627 118.627"
            ]
      ],

      HE.div [HA.class' "logo-contact-list", HA.onDblclick $ ToggleFortune true] $
            HE.img [
                  HA.createAttribute "srcset" "/client/media/logo-3-small.png 180w, /client/media/logo-small.png 210w",
                  HA.createAttribute "sizes" "(max-width: 1920px) 180px, 210px",
                  HA.src "/client/media/logo.png"
            ]
]