module Shared.IM.View.ProfileSettings where

import Shared.IM.Types
import Prelude

import Flame (Html)
import Flame.HTML.Attribute as HA
import Flame.HTML.Element as HE

profileSettings :: ProfileSettingsToggle -> Html IMMessage
profileSettings toggle = HE.div (HA.class' $ "profile-settings-placeholder" <> if toggle /= Hidden then "" else " hidden") [
      HE.div (HA.class' "profile-settings-menu") [
            HE.div [HA.onClick (ToggleProfileSettings Hidden)] [
                  HE.svg [HA.class' "svg-32 back-arrow", HA.id "cil-arrow-thick-to-left", HA.viewBox "0 0 24 24"] [
                        HE.path' $ HA.d "M15.75 8.25v-5.625h-1.81l-9.375 9.366 9.375 9.384h1.811v-5.625h7.5v-7.5zM21.75 14.25h-7.5v5.314l-7.564-7.572 7.564-7.557v5.315h7.5z",
                        HE.path' $ HA.d "M0.75 2.625h1.5v18.75h-1.5v-18.75z"
                  ],
                  HE.text "Back to chats"
            ],
            HE.div [HA.onClick (ToggleProfileSettings ShowProfile), HA.class' { green: toggle == ShowProfile }] "Your profile",
            HE.div [HA.onClick (ToggleProfileSettings ShowSettings), HA.class' { green: toggle == ShowSettings }] "Your settings"
      ],
      HE.div [HA.id "profile-edition-root", HA.class' { hidden: toggle /= ShowProfile }] $ "Loading...",
      HE.div [HA.id "settings-edition-root", HA.class' { hidden: toggle /= ShowSettings }] $ "Loading..."
]
