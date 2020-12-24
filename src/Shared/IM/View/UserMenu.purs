module Shared.IM.View.UserMenu where

import Prelude
import Shared.Types

import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Avatar as SA
import Shared.IM.Svg as SIS

userMenu :: IMModel -> Html IMMessage
userMenu model@{ toggleContextMenu, smallScreen} =
      HE.div (HA.class' "settings") [
            header model,
            HE.div [HA.class' "outer-user-menu"] [
                  HE.svg [HA.id "user-context-menu", HA.class' "svg-32 svg-user-menu-context", HA.viewBox "0 0 16 16"] (SIS.contextMenuElements <> [
                        HE.rect' [HA.class' "strokeless", HA.x "0.03", HA.y "7", HA.width "15.93", HA.height "2"],
                        HE.rect' [HA.class' "strokeless", HA.x "0.03", HA.y "2.5", HA.width "15.93", HA.height "2"],
                        HE.rect' [HA.class' "strokeless", HA.x "0.03", HA.y "11.5", HA.width "15.93", HA.height "2"]
                  ]),
                  HE.div [HA.class' {"user-menu": true, visible: toggleContextMenu == ShowUserContextMenu }][
                        HE.div (HA.class' "mobile-profile-header") $ header model,
                        HE.div [HA.class' "user-menu-item", HA.onClick <<< SpecialRequest $ ToggleModal ShowProfile] [
                              HE.div (HA.class' "menu-item-heading") "Profile",
                              HE.span (HA.class' "duller") "Set your profile picture, name"
                        ],
                        HE.div [HA.class' "user-menu-item", HA.onClick <<< SpecialRequest $ ToggleModal ShowSettings] [
                              HE.div (HA.class' "menu-item-heading") "Settings",
                              HE.span (HA.class' "duller") "Change email, password, etc"
                        ],
                        HE.div [HA.class' "user-menu-item", HA.onClick <<< SpecialRequest $ ToggleModal ShowLeaderboard] [
                              HE.div (HA.class' "menu-item-heading") "Karma leaderboard",
                              HE.span (HA.class' "duller") "See your karma rank and stats"
                        ],
                        HE.div [HA.class' "user-menu-item", HA.onClick <<< SpecialRequest $ ToggleModal ShowHelp] [
                              HE.div (HA.class' "menu-item-heading") "Help",
                              HE.span (HA.class' "duller") "Learn more about MelanChat"
                        ],
                        HE.div [HA.class' "user-menu-item logout menu-item-heading", HA.onClick <<< SpecialRequest $ ToggleModal ConfirmLogout] "Logout"
                  ]
            ],
            HE.span [HA.class' "suggestions-button", HA.onClick ToggleInitialScreen] "+"
      ]

header :: IMModel -> Html IMMessage
header { user: { name, avatar, karma, karmaPosition }} = HE.fragment [
      HE.img [HA.onClick <<< SpecialRequest $ ToggleModal ShowProfile, HA.title "Edit your profile", HA.class' "avatar-settings", HA.src $ SA.avatarForSender avatar],
      HE.div [HA.class' "settings-name"] [
            HE.strong_ name,
            HE.div [HA.class' "settings-karma", HA.onClick <<< SpecialRequest $ ToggleModal ShowLeaderboard, HA.title "See karma leaderboard"] [
                  HE.span [HA.class' "karma-number"] $ show karma,
                  HE.span [HA.class' "karma-text"] " karma ",
                  HE.span_ $ "(#" <> show karmaPosition <> ")"
            ]
      ]
]