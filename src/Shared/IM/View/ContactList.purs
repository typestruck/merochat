module Shared.IM.View.ContactList where

import Prelude
import Shared.Experiments.Types
import Shared.IM.Types

import Data.Array ((!!), (:))
import Data.Array as DA
import Data.Enum as DE
import Data.Foldable as DF
import Data.HashMap as DH
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Newtype as DN
import Data.Tuple (Tuple(..))
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Avatar as SA
import Shared.DateTime (DateTimeWrapper(..))
import Shared.DateTime as SD
import Shared.Experiments.Impersonation (impersonations)
import Shared.Experiments.Impersonation as SEI
import Shared.IM.Svg (backArrow, nextArrow)
import Shared.IM.View.Retry as SIVR
import Shared.Markdown as SM
import Debug
import Shared.Unsafe as SU
import Shared.User (ProfileVisibility(..))

-- | Users that have exchanged messages with the current logged in user
contactList ∷ Boolean → IMModel → Html IMMessage
contactList isClientRender { failedRequests, chatting, toggleContextMenu, experimenting, contacts, user: { id: loggedUserId, readReceipts, typingStatus, profileVisibility, messageTimestamps } } =
      case profileVisibility of
            Nobody → HE.div' [ HA.id $ show ContactList, HA.class' "contact-list" ]
            _ →
                  HE.div
                        [ HA.id $ show ContactList
                        , HA.onScroll CheckFetchContacts
                        , HA.class' "contact-list"
                        ]
                        $ retryLoadingNewContact : DA.snoc displayContactList retryLoadingContacts
      where
      -- | Contact list sorting is only done for the dom nodes, model.contacts is left unchanged
      displayContactList
            | DA.null contacts = [ suggestionsCall ]
            | otherwise =
                    DA.mapWithIndex displayContactListEntry
                          <<< DA.sortBy compareLastDate
                          $ DA.filter (not <<< DA.null <<< _.history) contacts -- might want to look into this: before sending a message, we need to run an effect; in this meanwhile history is empty

      displayContactListEntry index { history, user, impersonating, typing } =
            let
                  justIndex = Just index
                  --refactor: a neater way to do experiment that don't litter the code with case of
                  contact = case impersonating of
                        Just impersonationID → SU.fromJust $ DH.lookup impersonationID impersonations
                        _ → user
                  numberUnreadMessages = countUnread history
                  lastHistoryEntry = SU.fromJust $ DA.last history
                  tupleId = Tuple user.id impersonating
                  isContextMenuVisible = toggleContextMenu == ShowContactContextMenu tupleId
            in
                  HE.div
                        [ HA.class' { contact: true, "chatting-contact": chattingId == Just user.id && impersonatingId == impersonating }
                        , HA.onClick $ ResumeChat tupleId
                        ]
                        [ HE.div (HA.class' "avatar-contact-list-div")
                                [ HE.img [ HA.class' $ "avatar-contact-list" <> SA.avatarColorClass justIndex, HA.src $ SA.avatarForRecipient justIndex contact.avatar ]
                                ]
                        , HE.div [ HA.class' "contact-profile" ]
                                [ HE.span (HA.class' "contact-name") contact.name
                                , HE.div' [ HA.class' { "contact-list-last-message": true, hidden: typing }, HA.innerHtml $ SM.parseRestricted lastHistoryEntry.content ]
                                , HE.div [ HA.class' { "contact-list-last-message typing": true, hidden: not typing || not typingStatus || not user.typingStatus } ] $ HE.p_ "Typing..."
                                ]
                        , HE.div (HA.class' "contact-options")
                                [ HE.span (HA.class' { duller: true, invisible: not isClientRender || not messageTimestamps || not contact.messageTimestamps }) <<< SD.ago $ DN.unwrap lastHistoryEntry.date
                                , HE.div (HA.class' { "unread-messages": true, hidden: numberUnreadMessages == 0 }) <<< HE.span (HA.class' "unread-number") $ show numberUnreadMessages
                                , HE.div (HA.class' { "message-status-contact": true, duller: true, hidden: numberUnreadMessages > 0 || lastHistoryEntry.sender == user.id || not contact.readReceipts || not readReceipts || isContextMenuVisible }) $ show lastHistoryEntry.status
                                , HE.div [ HA.class' { "message-context-menu outer-user-menu": true, visible: isContextMenuVisible }, HA.onClick <<< SetContextMenuToggle $ ShowContactContextMenu tupleId ]
                                        [ HE.svg [ HA.class' "svg-32 svg-duller", HA.viewBox "0 0 16 16" ]
                                                [ HE.polygon' [ HA.transform "rotate(90,7.6,8)", HA.points "11.02 7.99 6.53 3.5 5.61 4.42 9.17 7.99 5.58 11.58 6.5 12.5 10.09 8.91 10.1 8.91 11.02 7.99" ]
                                                ]
                                        , HE.div [ HA.class' { "user-menu": true, visible: isContextMenuVisible }, HA.onClick <<< SpecialRequest <<< ToggleModal $ ConfirmDeleteChat tupleId ] $
                                                HE.div [ HA.class' "user-menu-item menu-item-heading" ] "Delete chat"
                                        ]
                                ]
                        ]

      -- | Since on mobile contact list takes most of the screen, show a welcoming message for new users/impersonations
      suggestionsCall =
            let
                  { welcome, first, second } = case experimenting of
                        Just (Impersonation (Just { name })) → SEI.welcomeMessage name
                        _ → welcomeMessage
            in
                  HE.div (HA.class' "suggestions-call")
                        [ HE.div (HA.onClick $ ToggleInitialScreen false) backArrow
                        , HE.div (HA.class' { "suggestions-call-middle": true, "welcome-impersonation": DM.isJust experimenting })
                                [ HE.div (HA.class' "welcome-suggestions-call") $ welcome
                                , HE.div_ first
                                , HE.div_ second
                                ]
                        , HE.div (HA.onClick $ ToggleInitialScreen false) nextArrow
                        ]

      welcomeMessage =
            { welcome: "Welcome!"
            , first: "Tap on either of the arrows to see "
            , second: "your chat suggestions"
            }

      compareLastDate contact anotherContact = compare anotherContact.lastMessageDate contact.lastMessageDate

      countUnread = DF.foldl unread 0

      unread total { status, sender } = total + DE.fromEnum (sender /= loggedUserId && status < Read)

      chattingId = do
            index ← chatting
            { user: { id } } ← contacts !! index
            pure id

      impersonatingId = do
            index ← chatting
            { impersonating } ← contacts !! index
            impersonating

      -- | Displayed if loading contact from an incoming message fails
      retryLoadingNewContact = SIVR.retry "Failed to sync contacts. You might have missed messages." CheckMissedEvents failedRequests

      -- | Displayed if loading contact list fails
      retryLoadingContacts = SIVR.retry "Failed to load contacts" (FetchContacts true) failedRequests
