module Shared.IM.View where

import Prelude
import Shared.Types

import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String.Common as DSC
import Effect (Effect)
import Flame (QuerySelector(..), Html)
import Flame as F
import Flame.HTML.Attribute as HA
import Flame.HTML.Element as HE

view :: IMModel -> Html IMMessage
view model@(IMModel { suggestions, chatting }) = HE.div (HA.class' "im") [
        HE.div_ [
                userMenu model,
                search model,
                contacts model
        ],
        HE.div (HA.class' "chat-box") [
                suggestion model $ DM.maybe Nothing (DA.index suggestions) chatting,
                history model,
                chat model
        ]
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

suggestion :: IMModel -> Maybe IMUser -> Html IMMessage
suggestion model =
        case _ of
                (Just (IMUser chatting)) ->
                        HE.div (HA.class' "suggestion") [
                                HE.a [HA.class' "skip", HA.title "you need more karma for that"] [
                                        HE.svg [HA.class' "i-start svg-50", HA.viewBox "0 0 32 32"] $
                                                HE.path' $ HA.d "M8 2 L8 16 22 2 22 30 8 16 8 30"
                                ],
                                HE.div (HA.class' "profile-info") [
                                        HE.div_ $ HE.img' [HA.class' "avatar-profile", HA.src chatting.avatar],
                                        HE.div_ [
                                                HE.h1_ chatting.name,
                                                HE.h3 (HA.class' "headline") chatting.headline
                                        ],
                                        HE.div_ $
                                                toInfoSpan false (map ((_ <> ",") <<< show) chatting.age) <>
                                                toInfoSpan true chatting.gender <>
                                                toInfoSpan true chatting.country <>
                                                --maybe include local time?
                                                (toInfoSpan false <<< maybeLanguages $ DSC.joinWith ", " chatting.languages),
                                        HE.div_ $ map toTagSpan chatting.tags
                                ],
                                HE.a [HA.class' "skip green", HA.title "See next profile", HA.onClick $ SM NextSuggestion] [
                                        HE.svg [HA.class' "i-end svg-50", HA.viewBox "0 0 32 32"] $
                                                HE.path' $ HA.d "M24 2 L24 16 10 2 10 30 24 16 24 30"
                                ]
                        ]
                _ ->
                        HE.div (HA.class' "suggestion") $ HE.div_ $ HE.img' $ HA.src "/client/media/logo.png"
        where   toInfoSpan includeSepator =
                        case _ of
                                Just s ->
                                        [HE.span_ $ s <> " "] <>
                                        (if includeSepator then
                                                [HE.span (HA.class' "smaller") "• "]
                                         else [])
                                _ -> [HE.createEmptyElement "span"]

                maybeLanguages =
                        case _ of
                                "" -> Nothing
                                l -> Just ("speaks " <> l)

                toTagSpan tag = HE.span (HA.class' "tag") tag

history :: IMModel -> Html IMMessage
history model = HE.div' (HA.class' "message-history")

chat :: IMModel -> Html IMMessage
chat model =
        HE.div (HA.class' "send-box") $
                HE.div (HA.class' "chat-input-textarea-options") $
                        HE.textarea' [HA.class' "chat-input-textarea", HA.placeholder "Type a message or drag files here"]

search model = HE.div' $ HA.class' "search"

contacts model = HE.div' $ HA.class' "contact-list"