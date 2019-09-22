module Shared.IM.View where

import Prelude
import Shared.Types

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Flame (QuerySelector(..), Html)
import Flame as F
import Flame.HTML.Attribute as HA
import Flame.HTML.Element as HE
import Data.String.Common as DSC

view :: IMModel -> Html IMMessage
view model = HE.div (HA.class' "im") [
        userMenu model,
        suggestion model
]

userMenu :: IMModel -> Html IMMessage
userMenu (IMModel {user: (IMUser user)}) =  HE.div [HA.id "settings", HA.class' "settings"][
        HE.a (HA.href "/settings/profile") $ HE.img' [HA.class' "avatar-settings", HA.src user.avatar],
        HE.div (HA.class' "settings-name") [
                HE.strong_ user.name,
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

suggestion :: IMModel -> Html IMMessage
suggestion (IMModel {chatting: Just (IMUser chatting)}) = HE.div (HA.class' "suggestion") [
        HE.a [HA.class' "skip", HA.title "you need more karma for that"] [
                HE.svg [HA.class' "i-start svg-50", HA.viewBox "0 0 32 32"] $
                        HE.path' $ HA.d "M8 2 L8 16 22 2 22 30 8 16 8 30"
        ],
        HE.div (HA.class' "profile-info") [
                HE.div_ $ HE.img' [HA.class' "avatar-profile", HA.src chatting.avatar],
                HE.h1_ $ chatting.name,
                HE.h3_ $ chatting.headline,
                HE.div_ $ maybeString (map show chatting.age) <> maybeString chatting.gender <> maybeString chatting.country <> DSC.joinWith ", " chatting.languages,
                HE.div_ $ DSC.joinWith " " chatting.tags
        ],
        HE.a [HA.class' "skip green", HA.title "See next profile"] [
                HE.svg [HA.class' "i-end svg-50", HA.viewBox "0 0 32 32"] $
                        HE.path' $ HA.d "M24 2 L24 16 10 2 10 30 24 16 24 30"
        ]
]
suggestion _ = HE.div (HA.class' "suggestion") $ HE.div_ $ HE.img' $ HA.src "/client/media/logo.png"

maybeString :: Maybe String -> String
maybeString (Just s) = s
maybeString _ = ""