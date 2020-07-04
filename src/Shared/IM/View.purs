module Shared.IM.View where

import Debug.Trace
import Prelude
import Shared.IM.Types

import Data.Array ((:))
import Data.Array as DA
import Data.Enum as DE
import Data.Foldable as DF
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Newtype as DN
import Data.String.Common as DSC
import Data.Tuple (Tuple(..))
import Flame (Html)
import Flame.HTML.Attribute as HA
import Flame.HTML.Attribute as HA
import Flame.HTML.Element as HE
import Shared.Markdown as SM
import Shared.Types (MDateTime(..))
import Shared.Unsafe ((!@))
import Shared.Unsafe as SU

--REFACTOR: split this into modules

view :: IMModel -> Html IMMessage
view model@(IMModel { suggestions, suggesting, chatting, contacts, profileSettingsToggle }) = HE.div (HA.class' "im") [
        HE.div (HA.class' "left-box") [
                userMenu model,
                search model,
                contactList model,

                profileSettings profileSettingsToggle
        ],
        HE.div (HA.class' "chat-box") [
                profile model profileUser,
                history model historyContact,
                chat model
        ]
]
        where Tuple profileUser historyContact =
                case Tuple suggesting chatting of
                        Tuple Nothing (Just index) ->
                                let contact@(Contact { user }) = contacts !@ index
                                 in Tuple (Just user) $ Just contact
                        Tuple (Just index) _ ->
                                let user@(IMUser { id }) = suggestions !@ index
                                 in Tuple (Just user) <<< Just $ Contact { user, history : [], chatStarter: id, chatAge : 0.0 }
                        _ -> Tuple Nothing Nothing

profileSettings :: ProfileSettingsToggle -> Html IMMessage
profileSettings toggle = HE.div (HA.class' $ "profile-settings-placeholder" <> if toggle /= Hidden then "" else " hidden") [
        HE.div (HA.class' "profile-settings-menu") [
                HE.div [HA.onClick (UMM $ ToggleProfileSettings Hidden)] [
                        HE.svg [HA.class' "svg-32 back-arrow", HA.id "cil-arrow-thick-to-left", HA.viewBox "0 0 24 24"] [
                                HE.path' $ HA.d "M15.75 8.25v-5.625h-1.81l-9.375 9.366 9.375 9.384h1.811v-5.625h7.5v-7.5zM21.75 14.25h-7.5v5.314l-7.564-7.572 7.564-7.557v5.315h7.5z",
                                HE.path' $ HA.d "M0.75 2.625h1.5v18.75h-1.5v-18.75z"
                        ],
                        HE.text "Back to chats"
                ],
                HE.div [HA.onClick (UMM $ ToggleProfileSettings ShowProfile), HA.class' { green: toggle == ShowProfile }] "Your profile",
                HE.div [HA.onClick (UMM $ ToggleProfileSettings ShowSettings), HA.class' { green: toggle == ShowSettings }] "Your settings"
        ],
        HE.div [HA.id "profile-edition-root", HA.class' { hidden: toggle /= ShowProfile }] $ "Loading...",
        HE.div [HA.id "settings-edition-root", HA.class' { hidden: toggle /= ShowSettings }] $ "Loading..."
]

userMenu :: IMModel -> Html IMMessage
userMenu (IMModel { user: (IMUser user), userContextMenuVisible }) =  HE.div [HA.id "settings", HA.class' "settings"][
        HE.a (HA.onClick (UMM $ ToggleProfileSettings ShowProfile)) $ HE.img [HA.class' "avatar-settings", HA.src user.avatar],
        HE.div (HA.class' "settings-name") [
                HE.strong_ user.name,
                HE.br,
                HE.text $ "Karma: 9001 (2) (1) (5)"
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
                       HE.a [HA.class' "menu-button", HA.onClick (UMM $ ToggleProfileSettings ShowProfile)] "Profile",
                       HE.a [HA.class' "menu-button", HA.onClick (UMM $ ToggleProfileSettings ShowSettings)] "Settings",
                       HE.a [HA.class' "menu-button", HA.onClick (UMM ConfirmLogout)] "Logout"
                ]
        ]
]

profile :: IMModel -> Maybe IMUser -> Html IMMessage
profile model =
        case _ of
                (Just (IMUser user)) ->
                        HE.div (HA.class' "suggestion") [
                                HE.a [HA.class' "skip", HA.title "See previous profile again", HA.onClick $ SM PreviousSuggestion] [
                                        HE.svg [HA.id "cil-arrow-thick-from-right", HA.viewBox "0 0 24 24", HA.class' "svg-50"] [
                                                HE.path' $ HA.d "M11.936 2.625h-1.811l-9.375 9.384 9.375 9.366h1.81v-5.625h6.75v-7.5h-6.75zM17.186 9.75v4.5h-6.75v5.315l-7.564-7.557 7.564-7.572v5.314z",
                                                HE.path' $ HA.d "M21.686 2.625h1.5v18.75h-1.5v-18.75z"
                                        ]
                                ],
                                HE.div (HA.class' "profile-info") [
                                        HE.div_ $ HE.img [HA.class' "avatar-profile", HA.src user.avatar],
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
                                        HE.svg [HA.id "cil-arrow-thick-from-left", HA.class' "svg-50", HA.viewBox "0 0 24 24"] [
                                                HE.path' $ HA.d "M13.875 2.625h-1.811v5.625h-6.75v7.5h6.75v5.625h1.81l9.375-9.366zM13.564 19.565v-5.315h-6.75v-4.5h6.75v-5.314l7.564 7.572z",
                                                HE.path' $ HA.d "M0.814 2.625h1.5v18.75h-1.5v-18.75z"
                                        ]
                                ]
                        ]
                _ ->
                        HE.div (HA.class' "suggestion") $ HE.div_ $ HE.img $ HA.src "/client/media/logo.png"
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

history :: IMModel -> Maybe Contact -> Html IMMessage
history (IMModel {user: (IMUser sender)}) chattingSuggestion = HE.div (HA.class' "message-history") <<< HE.div (HA.class' "message-history-wrapper") $
        case chattingSuggestion of
                Nothing -> [HE.createEmptyElement "div"]
                Just recipient -> display recipient

        where   entry ({ id: senderID, avatar: senderAvatar }) recipientAvatar (HistoryMessage { sender, content }) =
                        let Tuple class' avatar =
                                if senderID == sender then Tuple "sender-message" senderAvatar
                                 else Tuple "recipient-message" recipientAvatar
                        in HE.div (HA.class' $ "message " <> class') [
                                HE.img [HA.src avatar, HA.class' "avatar-message"],
                                HE.div' [HA.innerHTML $ SM.toHTML content]
                        ]

                display (Contact recipient@{history, user: IMUser { description, avatar }}) =
                        --having the description node always present avoids snabbdom choking on the use of innerHTML
                        HE.div' [HA.class' {"message": true, "description-message" : true, "hidden": not $ DA.null history }, HA.innerHTML $ SM.toHTML description] : map (entry sender avatar) history

chat :: IMModel -> Html IMMessage
chat (IMModel {chatting, suggesting}) =
        HE.div (HA.class' "send-box") $
                let classes = "chat-input-textarea-options" <> if DM.isNothing chatting && DM.isNothing suggesting then " hidden" else ""
                 in HE.div (HA.class' classes) $
                        HE.textarea' [HA.class' "chat-input-textarea", HA.placeholder "Type a message or drag files here"]

search model = HE.div' $ HA.class' "search"

contactList :: IMModel -> Html IMMessage
contactList (IMModel { contacts, user: IMUser { id: userID } }) = HE.div [HA.onWheel' (CNM <<< FetchContacts),  HA.class' "contact-list"] <<< map contactEntry $ DA.sortBy compareDates contacts
        where   getDate history = do
                        HistoryMessage { date: MDateTime md } <- DA.last history
                        pure md
                compareDates (Contact contact) (Contact anotherContact) = compare (getDate anotherContact.history) (getDate contact.history)

                countUnread total (HistoryMessage { status, sender }) = total + DE.fromEnum (sender /= userID && status == Unread)
                showUnreadCount history' = let count = DF.foldl countUnread 0 history' in if count == 0 then "" else show count
                --should only work for text messages!
                lastMessage = DM.maybe "" (SM.toRestrictedHTML <<< _.content <<< DN.unwrap) <<< DA.last

                contactEntry (Contact { history, user: IMUser { id, name, avatar, headline }}) =
                        HE.div [HA.class' "contact", HA.onClick <<< CNM $ ResumeChat id] [
                                HE.img [HA.class' "avatar-contact-list", HA.src avatar],
                                HE.div [HA.class' "contact-profile"] [
                                        HE.strong_ name,
                                        HE.br,
                                        HE.div' [HA.class' "contact-list-last-message", HA.innerHTML $ lastMessage history]
                                ],
                                HE.div (HA.class' "menu-button chat-options") [
                                        HE.text $ showUnreadCount history,
                                        HE.a (HA.class' "menu-button") $
                                                HE.svg [HA.class' "i-chevron-bottom svg-16 svg-right", HA.viewBox "0 0 32 32"] $
                                                        HE.path' (HA.d "M30 12 L16 24 2 12"),
                                        HE.div' (HA.class' "drop-menu fade-in effect")
                                ]
                        ]