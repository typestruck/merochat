module Shared.IM.View.UserMenu where

import Prelude
import Shared.Types

import Data.Maybe as DM
import Flame (Html)
import Flame.HTML.Attribute as HA
import Flame.HTML.Element as HE
import Shared.Avatar as SA
import Shared.Routes (routes)

userMenu :: IMModel -> Html IMMessage
userMenu { user: { name, avatar, karma, karmaPosition }, userContextMenuVisible } =  HE.div [HA.id "settings", HA.class' "settings"][
      HE.a [HA.onClick (ToggleProfileSettings ShowProfile)] $ HE.img [HA.title "Edit your profile", HA.class' "avatar-settings", HA.src $ SA.avatarForSender avatar],
      HE.div [HA.class' "settings-name"] [
            HE.div [HA.class' "settings-karma", HA.onClick ToggleKarmaLeaderBoard, HA.title "See karma leaderboard"] [
                  HE.span [HA.class' "karma-number"] $ show karma,
                  HE.span [HA.class' "karma-text"] " karma ",
                  HE.span_ $ "(#" <> show karmaPosition <> ")"
            ]
      ],
      HE.div [HA.class' "outer-user-menu"] [
            HE.svg [HA.id "user-context-menu", HA.class' "svg-32", HA.viewBox "0 0 32 32"][
                  HE.circle' [HA.cx "16", HA.cy "7", HA.r "2"],
                  HE.circle' [HA.cx "16", HA.cy "16", HA.r "2"],
                  HE.circle' [HA.cx "16", HA.cy "25", HA.r "2"]
            ],
            HE.div [HA.class' $ "user-menu " <> if userContextMenuVisible then "visible" else ""][
                  HE.div [HA.class' "user-menu-item", HA.onClick (ToggleProfileSettings ShowProfile)] [
                        HE.div (HA.class' "menu-item-heading") "Profile",
                        HE.span (HA.class' "duller") "Set your profile picture, name"
                  ],
                  HE.div [HA.class' "user-menu-item", HA.onClick (ToggleProfileSettings ShowSettings)] [
                        HE.div (HA.class' "menu-item-heading") "Settings",
                        HE.span (HA.class' "duller") "Change email, password, etc"
                  ],
                  HE.div [HA.class' "user-menu-item", HA.onClick ToggleKarmaLeaderBoard] [
                        HE.div (HA.class' "menu-item-heading") "Karma leaderboard",
                        HE.span (HA.class' "duller") "See your karma rank and stats"
                  ],
                  HE.a [HA.class' "user-menu-item", HA.href $ routes.help {}, HA.target "_blank" ] [
                        HE.div (HA.class' "menu-item-heading") "Help",
                        HE.span (HA.class' "duller") "Learn more about MelanChat"
                  ],
                  HE.div [HA.class' "user-menu-item logout menu-item-heading", HA.onClick ConfirmLogout] "Logout"
            ]
      ]
]