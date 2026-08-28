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
      | model.smallScreen = HE.div [ HA.class' "relative" ]
              [ HE.div [ HA.class' "suggestion-mobile-button", HA.onClick $ ToggleInitialScreen false ]
                      [ HE.i_ [ HE.text "Start new chat" ]
                      , nextArrow
                      ]
              ]
      | otherwise = HE.div [ HA.class' "relative" ]
              [ HE.div [ HA.class' { fortune: true, hidden: DM.isNothing model.fortune || model.collapsedSidebar } ]
                      [ HE.div [ HA.class' "fortune-deets" ]
                              [ HE.text $ DM.fromMaybe "" model.fortune
                              ]
                      , HE.svg [ HA.viewBox "0 0 512 512", HA.onClick (ToggleFortune false) ]
                              [ HE.title [ HE.text "Close" ]
                              , HE.polygon' [ HA.points "438.627 118.627 393.373 73.373 256 210.746 118.627 73.373 73.373 118.627 210.746 256 73.373 393.373 118.627 438.627 256 301.254 393.373 438.627 438.627 393.373 301.254 256 438.627 118.627" ]
                              ]
                      ]

              , HE.div [ HA.class' "logo-contact-list" ]
                      $
                            if model.collapsedSidebar then
                                  [ HE.svg [ HA.class' "inverted-merochat-svg collapsed", HA.viewBox "0 0 30 22", HA.fill "none" ]
                                          [ HE.path' [ HA.fillRule "evenodd", HA.clipRule "evenodd", HA.d "M20.058 18.151C17.968 20.507 14.863 22 11.397 22 5.104 22 0 17.075 0 11S5.104 0 11.397 0c6.296 0 11.397 4.925 11.397 11 0 1.22-.206 2.395-.586 3.493l2.344 4.243zm.118-4.013c-2.023 5.046-7.67 7.53-12.613 5.549C2.617 17.705.248 12.009 2.268 6.962l2.168.87A2.826 2.826 0 0 0 8 10.231a2.827 2.827 0 0 0 4.84.967zM6.71 13.412a.668.668 0 1 1-1.337-.001.668.668 0 0 1 1.337 0m3.67 1.155a.668.668 0 1 1-1.336 0 .668.668 0 0 1 1.336 0m3.295 2.153a.667.667 0 1 0 0-1.334.667.667 0 0 0 0 1.334m75.586 2.464" ]
                                          ]
                                  ]
                            else
                                  [ SISP.miniSuggestions model
                                  , SIS.invertedMerochat [ HA.class' "inverted-merochat-svg", HA.viewBox "0 0 122 22", HA.fill "none", HA.onDblclick $ ToggleFortune true ]
                                  ]
              ]
