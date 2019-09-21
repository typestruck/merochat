module Shared.IM.View where

import Prelude

import Effect (Effect)
import Flame (QuerySelector(..), Html)
import Flame as F
import Flame.HTML.Attribute as HA
import Flame.HTML.Element as HE
import Shared.Types

view :: IMModel -> Html IMMessage
view (IMModel {user: (IMUser model)}) = HE.div (HA.class' "im") [
        userMenu
]
        where userMenu =  HE.div [HA.id "settings", HA.class' "settings"][
                HE.a (HA.href "/settings/profile") $ HE.img' [HA.class' "avatar-settings", HA.src model.avatar],
                HE.div (HA.class' "settings-name") [
                        HE.strong_ model.name,
                        HE.br,
                        HE.text $ "Karma: 9001 (2) (1) (5)"
                ],
                HE.div (HA.class' "menu-button outer-drop-menu") [
                        HE.a [HA.class' "menu-button"] [
                                HE.svg [HA.class' "svg-right i-ellipsis-vertical svg-32 svg-more", HA.viewBox "0 0 32 32"][
                                        HE.circle' [HA.cx "16", HA.cy "7", HA.r "2"],
                                        HE.circle' [HA.cx "16", HA.cy "16", HA.r "2"],
                                        HE.circle' [HA.cx "16", HA.cy "25", HA.r "2"]
                                ]
                        ],
                        HE.div [HA.class' "drop-menu fade-in effect"][
                                HE.a [HA.class' "menu-button", HA.href "/settings/profile"] "Profile",
                                HE.a [HA.class' "menu-button", HA.href "/settings"] "Settings",
                                HE.i_ "🍉",
                                HE.a (HA.href "#") "Help",
                                HE.a [HA.class' "menu-button", HA.href "#"] "Become a backer",
                                HE.i_ "🍉",
                                HE.a [HA.class' "menu-button"] "Logout"
                        ]
                ]
            ]