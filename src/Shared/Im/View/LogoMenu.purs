module Shared.Im.View.LogoMenu where

import Prelude

import Data.Maybe as DM
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Im.Svg (nextArrow)
import Shared.Im.Svg as SIS
import Shared.Im.Types (ImMessage(..), ImModel)
import Shared.Im.View.Profile as SISP

logoMenu ∷ ImModel → Html ImMessage
logoMenu model
      | model.smallScreen = HE.div [HA.class' "relative"]
              [ HE.div [ HA.class' "suggestion-mobile-button", HA.onClick $ ToggleInitialScreen false ]
                      [ HE.i_ [HE.text "Start new chat"]
                      , nextArrow
                      ]
              ]
      | otherwise = HE.div [HA.class' "relative"]
              [ HE.div [HA.class' { fortune: true, hidden: DM.isNothing model.fortune }]
                      [ HE.div [HA.class' "fortune-deets"]
                              [ HE.text $ DM.fromMaybe "" model.fortune
                              ]
                      , HE.svg [ HA.viewBox "0 0 512 512", HA.onClick (ToggleFortune false) ]
                              [ HE.title [HE.text "Close"]
                              , HE.polygon' [HA.points "438.627 118.627 393.373 73.373 256 210.746 118.627 73.373 73.373 118.627 210.746 256 73.373 393.373 118.627 438.627 256 301.254 393.373 438.627 438.627 393.373 301.254 256 438.627 118.627"]
                              ]
                      ]
              , HE.div [ HA.class' "logo-contact-list" ]
                      [ SISP.miniSuggestions model
                      , SIS.invertedMerochat [ HA.class' "inverted-merochat-svg", HA.viewBox "0 0 122 22", HA.fill "none", HA.onDblclick $ ToggleFortune true ]
                      ]
              ]
