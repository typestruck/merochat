module Shared.IM.View.UserMenu where

import Prelude
import Shared.IM.Types

import Flame (Html)
import Flame.HTML.Attribute as HA
import Flame.HTML.Element as HE
import Shared.Avatar as SA

userMenu :: IMModel -> Html IMMessage
userMenu (IMModel { user: IMUser { name, avatar, karma }, userContextMenuVisible }) =  HE.div [HA.id "settings", HA.class' "settings"][
      HE.a (HA.onClick (ToggleProfileSettings ShowProfile)) $ HE.img [HA.class' "avatar-settings", HA.src $ SA.avatarForSender avatar],
      HE.div (HA.class' "settings-name") [
            HE.strong_ name,
            HE.br,
            HE.text $ "Karma: " <> show karma
      ],
      HE.div [HA.class' $ "menu-button outer-drop-menu" <> if userContextMenuVisible then " dropdown-wrapper-visible" else ""] [
            HE.a [HA.class' "menu-button" ] [
                  HE.svg [HA.id "user-context-menu", HA.class' "svg-right i-ellipsis-vertical svg-32 svg-more", HA.viewBox "0 0 32 32"][
                        HE.circle' [HA.cx "16", HA.cy "7", HA.r "2"],
                        HE.circle' [HA.cx "16", HA.cy "16", HA.r "2"],
                        HE.circle' [HA.cx "16", HA.cy "25", HA.r "2"]
                  ]
            ],
            HE.div [HA.class' "drop-menu fade-in effect"][
                   HE.a [HA.class' "menu-button", HA.onClick (ToggleProfileSettings ShowProfile)] "Profile",
                   HE.a [HA.class' "menu-button", HA.onClick (ToggleProfileSettings ShowSettings)] "Settings",
                   HE.a [HA.class' "menu-button", HA.onClick ConfirmLogout] "Logout"
            ]
      ]
]