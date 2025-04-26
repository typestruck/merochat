module Shared.Im.View.ContactList where

import Prelude
import Shared.Availability
import Shared.Experiments.Types
import Shared.Im.Types

import Data.Array ((!!), (:))
import Data.Array as DA
import Data.Enum as DE
import Data.Foldable as DF

import Data.Maybe (Maybe(..))

import Data.Newtype as DN
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Avatar as SA
import Shared.DateTime as SD
import Shared.Element (ElementId(..))
import Shared.Im.Contact as SIC
import Shared.Im.Svg (backArrow, nextArrow)
import Shared.Im.View.Retry as SIVR
import Shared.Im.View.SuggestionProfile as SIVP
import Shared.Markdown as SM
import Shared.Unsafe as SU
import Shared.User (ProfileVisibility(..))

-- | Users that have exchanged messages with the current logged in user
contactList ∷ Boolean → ImModel → Html ImMessage
contactList
      isClientRender
      model@
            { failedRequests
            , toggleContextMenu
            , contacts
            , toggleModal
            , user: { joined, temporary, id: loggedUserId, readReceipts, typingStatus, profileVisibility, messageTimestamps, onlineStatus }
            } =
      case profileVisibility of
            Nobody → HE.div' [ HA.id $ show ContactList, HA.class' "contact-list" ]
            _ →
                  HE.div
                        [ HA.id $ show ContactList
                        , HA.onScroll CheckFetchContacts
                        , HA.class' { "contact-list": true, highlighted: toggleModal == Tutorial ChatList }
                        ]
                        $ retryLoadingNewContact : DA.snoc displayContactList retryLoadingContacts
      where
      -- | Contact list sorting is only done for the dom nodes, model.contacts is left unchanged
      displayContactList
            | DA.null contacts = [ suggestionsCall ]
            | otherwise =
                    let
                          entries =
                                map displayContactListEntry
                                      <<< DA.sortBy compareLastDate
                                      $ DA.filter (not <<< DA.null <<< _.history) contacts
                    in
                          if temporary then SIVP.signUpCall joined : entries else entries

      displayContactListEntry contact =
            let
                  numberUnreadMessages = countUnread contact.history
                  lastHistoryEntry = SU.fromJust $ DA.last contact.history
                  isContextMenuVisible = toggleContextMenu == ShowContactContextMenu contact.user.id
            in
                  HE.div (HA.class' "contact-wrapper")
                        [ HE.div
                                [ HA.class' { contact: true, "chatting-contact": chattingId == Just contact.user.id }
                                , HA.onClick $ ResumeChat contact.user.id
                                ]
                                [ HE.div [ HA.class' "avatar-contact-list-div", HA.title $ if contact.user.onlineStatus && onlineStatus then show contact.user.availability else "" ]
                                        [ HE.img [ SA.async, SA.decoding "lazy", HA.class' "avatar-contact-list", HA.src $ SA.fromAvatar contact.user.avatar ]
                                        ]
                                , HE.div [ HA.class' "contact-profile" ]
                                        [ HE.div (HA.class' "contact-online-wrapper")
                                                [ HE.span (HA.class' "contact-name") contact.user.name
                                                , HE.div' [ HA.class' { "online-indicator": true, hidden: contact.user.availability /= Online || not contact.user.onlineStatus || not onlineStatus } ]
                                                ]
                                        , HE.div' [ HA.class' { "contact-list-last-message duller": true, hidden: contact.typing && typingStatus && contact.user.typingStatus }, HA.innerHtml $ SM.parseRestricted lastHistoryEntry.content ]
                                        , HE.div [ HA.class' { "contact-list-last-message duller typing": true, hidden: not contact.typing || not typingStatus || not contact.user.typingStatus } ] $ HE.p_ "Typing..."
                                        ]
                                , HE.div (HA.class' "contact-options")
                                        [ HE.span (HA.class' { duller: true, invisible: not isClientRender || not messageTimestamps || not contact.user.messageTimestamps }) <<< SD.ago $ DN.unwrap lastHistoryEntry.date
                                        , HE.div (HA.class' { "unread-messages": true, hidden: numberUnreadMessages == 0 }) <<< HE.span (HA.class' "unread-number") $ show numberUnreadMessages
                                        , HE.div (HA.class' { "message-status-contact duller": true, hidden: numberUnreadMessages > 0 || lastHistoryEntry.sender == contact.user.id || not contact.user.readReceipts || not readReceipts || isContextMenuVisible }) $ show lastHistoryEntry.status
                                        , HE.div [ HA.class' { "message-context-menu outer-user-menu": true, visible: isContextMenuVisible }, HA.onClick <<< SetContextMenuToggle $ ShowContactContextMenu contact.user.id ]
                                                [ HE.svg [ HA.class' "svg-32 svg-duller", HA.viewBox "0 0 16 16" ]
                                                        [ HE.polygon' [ HA.transform "rotate(90,7.6,8)", HA.points "11.02 7.99 6.53 3.5 5.61 4.42 9.17 7.99 5.58 11.58 6.5 12.5 10.09 8.91 10.1 8.91 11.02 7.99" ]
                                                        ]
                                                , HE.div [ HA.class' { "user-menu": true, visible: isContextMenuVisible }, HA.onClick <<< SpecialRequest <<< ToggleModal $ ConfirmDeleteChat contact.user.id ] $
                                                        HE.div [ HA.class' "user-menu-item menu-item-heading" ] "Delete chat"
                                                ]
                                        ]

                                ]
                        , HE.hr' (HA.class' "contact-ruler")
                        ]

      -- | Since on mobile contact list takes most of the screen, show a welcoming message for new users
      suggestionsCall =
            let
                  { welcome, first, second } = welcomeMessage
            in
                  HE.div (HA.class' "suggestions-call")
                        [ HE.div (HA.onClick $ ToggleInitialScreen false) backArrow
                        , HE.div (HA.class' { "suggestions-call-middle": true })
                                [ HE.div (HA.class' "welcome-suggestions-call") $ welcome
                                , HE.div_ first
                                , HE.div_ second
                                ]
                        , HE.div (HA.onClick $ ToggleInitialScreen false) nextArrow
                        ]

      welcomeMessage =
            { welcome: "Welcome!"
            , first: "Tap on either of the arrows to see "
            , second: "new users to chat to"
            }

      compareLastDate contact anotherContact = compare anotherContact.lastMessageDate contact.lastMessageDate

      countUnread = DF.foldl unread 0
      unread total ss = total + DE.fromEnum (ss.sender /= loggedUserId && ss.status < Read)

      chattingId = (_.id <<< _.user) <$> SIC.maybeFindContact model.chatting model.contacts

      -- | Displayed if loading contact from an incoming message fails
      retryLoadingNewContact = SIVR.retry "Failed to sync contacts. You might have missed messages." (CheckMissedEvents Nothing) failedRequests

      -- | Displayed if loading contact list fails
      retryLoadingContacts = SIVR.retry "Failed to load contacts" (FetchContacts true) failedRequests
