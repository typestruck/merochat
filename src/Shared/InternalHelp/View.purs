module Shared.InternalHelp.View where

import Prelude

import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Server.Faq as SH
import Server.Privacy as SP

import Server.Terms as ST
import Shared.InternalHelp.Types (DisplayHelpSection(..), InternalHelpMessage(..), InternalHelpModel)

view ∷ InternalHelpModel → Html InternalHelpMessage
view model@{ toggleHelp } =
      HE.div [ HA.class' { hidden: not model.visible }, HA.id "internal-help" ]
            [ HE.div [ HA.class' "help" ]
                    [ HE.div_
                            [ HE.div [ HA.class' "green-tab" ]
                                    [ HE.span [ HA.class' { "regular-green-tab": true, "selected-green-tab": toggleHelp == FAQ }, HA.onClick $ ToggleHelpSection FAQ ] [ HE.text "FAQ" ]
                                    , HE.span [ HA.class' { "regular-green-tab": true, "selected-green-tab": toggleHelp == Terms }, HA.onClick $ ToggleHelpSection Terms ] [ HE.text "Terms and conditions" ]
                                    , HE.span [ HA.class' { "regular-green-tab": true, "selected-green-tab": toggleHelp == Privacy }, HA.onClick $ ToggleHelpSection Privacy ] [ HE.text "Privacy policy" ]
                                    ]
                            , HE.div [ HA.class' { "hidden": toggleHelp /= FAQ } ] [ SH.faq ]
                            , HE.div [ HA.class' { "hidden": toggleHelp /= Terms } ] [ ST.terms ]
                            , HE.div [ HA.class' { "hidden": toggleHelp /= Privacy } ] [ SP.privacy ]
                            ]
                    ]
            ]
