module Shared.IM.View where

import Prelude
import Shared.Types
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String.Common as DSC
import Shared.Unsafe((!@))
import Flame (Html)
import Data.Tuple(Tuple(..))
import Flame.HTML.Attribute as HA
import Data.Int53 as DI
import Debug.Trace (spy)
import Data.Enum as DE
import Data.Foldable as DF
import Debug.Trace(spy)
import Flame.HTML.Element as HE
import Data.Array as DA

view :: IMModel -> Html IMMessage
view model@(IMModel { suggestions, suggesting, chatting, contacts }) = HE.div (HA.class' "im") [
        HE.div_ [
                userMenu model,
                search model,
                contactList model
        ],
        HE.div (HA.class' "chat-box") [
                profile model chattingOrSuggesting,
                history model chattingOrSuggesting,
                chat model
        ]
]
        where chattingOrSuggesting =
                case Tuple chatting suggesting of
                        Tuple Nothing (Just index) -> Just (suggestions !@ index)
                        Tuple (Just index) _ -> Just (contacts !@ index)
                        _ -> Nothing

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

profile :: IMModel -> Maybe IMUser -> Html IMMessage
profile model =
        case _ of
                (Just (IMUser user)) ->
                        HE.div (HA.class' "suggestion") [
                                HE.a [HA.class' "skip", HA.title "you need more karma for that"] [
                                        HE.svg [HA.class' "i-start svg-50", HA.viewBox "0 0 32 32"] $
                                                HE.path' $ HA.d "M8 2 L8 16 22 2 22 30 8 16 8 30"
                                ],
                                HE.div (HA.class' "profile-info") [
                                        HE.div_ $ HE.img' [HA.class' "avatar-profile", HA.src user.avatar],
                                        HE.div_ [
                                                HE.h1_ user.name,
                                                HE.h3 (HA.class' "headline") user.headline
                                        ],
                                        HE.div_ $
                                                toInfoSpan false (map ((_ <> ",") <<< show) user.age) <>
                                                toInfoSpan true user.gender <>
                                                toInfoSpan true user.country <>
                                                --maybe include local time?
                                                (toInfoSpan false <<< maybeLanguages $ DSC.joinWith ", " user.languages),
                                        HE.div_ $ map toTagSpan user.tags
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

history :: IMModel -> Maybe IMUser -> Html IMMessage
history (IMModel {user: (IMUser sender)}) chattingSuggestion = HE.div (HA.class' "message-history") <<< HE.div (HA.class' "message-history-wrapper") $
        case chattingSuggestion of
                Nothing -> [HE.createEmptyElement "div"]
                Just recipient -> display recipient

        where   entry ({id: senderID, avatar: senderAvatar}) {avatar: recipientAvatar} (HistoryMessage {sender, content}) =
                        let Tuple class' avatar =
                                if senderID == sender then Tuple "sender-message" senderAvatar
                                 else Tuple "recipient-message" recipientAvatar
                        in HE.div (HA.class' $ "message " <> class') [
                                HE.img' [HA.src avatar, HA.class' "avatar-message"],
                                HE.text content
                        ]

                display (IMUser recipient@{history, description})
                        | DA.null history = [HE.div (HA.class' "message description-message") description]
                        | otherwise = map (entry sender recipient) history

chat :: IMModel -> Html IMMessage
chat (IMModel {chatting, suggesting}) =
        HE.div (HA.class' "send-box") $
                let classes = "chat-input-textarea-options" <> if DM.isNothing chatting && DM.isNothing suggesting then " hidden" else ""
                 in HE.div (HA.class' classes) $
                        HE.textarea' [HA.class' "chat-input-textarea", HA.placeholder "Type a message or drag files here"]

search model = HE.div' $ HA.class' "search"

contactList :: IMModel -> Html IMMessage
contactList (IMModel { contacts, user: IMUser { id: userID } }) = HE.div (HA.class' "contact-list") <<< map contactEntry $ DA.sortBy compareDates contacts
        where   getDate history = do
                        HistoryMessage {date} <- DA.last history
                        MDateTime md <- date
                        pure md
                compareDates (IMUser user) (IMUser anotherUser) = compare (getDate anotherUser.history) (getDate user.history)

                countUnread total (HistoryMessage {status, sender}) = total + DE.fromEnum (sender /= userID && status == Unread)
                showUnreadCount history = let count = DF.foldl countUnread 0 history in if count == 0 then "" else show count

                contactEntry (IMUser { id, name, avatar, headline, history }) =
                        HE.div [HA.class' "contact", HA.onClick <<< CNM $ ResumeChat id] [
                                HE.img' [HA.class' "avatar-contact-list", HA.src avatar],
                                HE.div (HA.class' "contact-profile") [
                                        HE.strong_ name,
                                        HE.br,
                                        --maybe the last sent message?
                                        HE.i (HA.class' "contact-list-description") headline
                                ],
                                HE.div (HA.class' "menu-button chat-options") [
                                        HE.text $ showUnreadCount history,
                                        HE.a (HA.class' "menu-button") $
                                                HE.svg [HA.class' "i-chevron-bottom svg-16 svg-right", HA.viewBox "0 0 32 32"] $
                                                        HE.path' (HA.d "M30 12 L16 24 2 12"),
                                        HE.div' (HA.class' "drop-menu fade-in effect")
                                ]
                        ]