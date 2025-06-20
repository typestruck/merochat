module Shared.Im.View.ContactList where

import Prelude
import Shared.Availability
import Shared.Experiments.Types
import Shared.Im.Types

import Data.Array ((:))
import Data.Array as DA
import Data.Enum as DE
import Data.Foldable as DF
import Data.Maybe (Maybe(..))
import Data.Newtype as DN
import Data.String as DS
import Debug (spy)
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Avatar as SA
import Shared.Backer.Contact (backerId)
import Shared.DateTime as SD
import Shared.Im.Contact as SIC
import Shared.Im.Scroll as SIS
import Shared.Im.Svg (backArrow, nextArrow)
import Shared.Im.View.Retry as SIVR
import Shared.Im.View.SuggestionProfile as SIVP
import Shared.Markdown as SM
import Shared.Unsafe as SU
import Shared.User (ProfileVisibility(..))

-- | Users that have exchanged messages with the current logged in user
contactList ∷ Boolean → ImModel → Html ImMessage
contactList isClientRender model =
      case model.user.profileVisibility of
            Nobody → HE.div' [ HA.class' "contact-list" ]
            _ →
                  HE.div
                        [ SIS.onScrollEvent CheckFetchContacts
                        , HA.class' { "contact-list": true, highlighted: model.toggleModal == Tutorial ChatList }
                        ]
                        (retryLoadingNewContact : DA.snoc displayContactList retryLoadingContacts)
      where
      -- | Contact list sorting is only done for the dom nodes, model.contacts is left unchanged
      displayContactList
            | DA.null model.contacts = [ suggestionsCall ]
            | otherwise =
                    let
                          entries = map displayContactListEntry $ DA.sortBy compareLastDate model.contacts
                    in
                          if model.user.temporary then SIVP.signUpCall model.user.joined : entries else entries

      displayContactListEntry contact =
            let
                  numberUnreadMessages = countUnread contact.history
                  lastHistoryEntry = SU.fromJust $ DA.last contact.history
                  backingCall = contact.user.id == backerId
            in
                  HE.div [ HA.class' "contact-wrapper" ]
                        [ HE.div
                                [ HA.class' { contact: true, "chatting-contact": chattingId == Just contact.user.id }
                                , HA.onClick $ if backingCall then SpecialRequest (ToggleModal ShowBacker) else ResumeChat contact.user.id
                                ]
                                [ HE.div [ HA.class' "avatar-contact-list-div", HA.title $ if contact.user.onlineStatus && model.user.onlineStatus then show contact.user.availability else "" ]
                                        [ HE.img [ SA.async, SA.decoding "lazy", HA.class' "avatar-contact-list", HA.src $ SA.fromAvatar contact.user ]
                                        ]
                                , HE.div [ HA.class' "contact-profile" ]
                                        [ HE.div (HA.class' "contact-online-wrapper")
                                                [ HE.span (HA.class' "contact-name") contact.user.name
                                                , HE.div' [ HA.class' { "online-indicator": true, hidden: contact.user.availability /= Online || not contact.user.onlineStatus || not model.user.onlineStatus } ]
                                                ]
                                        , if contact.typing && model.user.typingStatus && contact.user.typingStatus then
                                                HE.div [ HA.class' "contact-list-last-message duller typing" ] [ HE.p_ "Typing..." ]
                                          else if not $ DS.null contact.draft then
                                                HE.div' [ HA.class' "contact-list-last-message duller message-draft", HA.innerHtml $ SM.parseRestricted ("Draft: " <> contact.draft) ]
                                          else
                                                HE.div' [ HA.class' "contact-list-last-message duller", HA.innerHtml $ SM.parseRestricted lastHistoryEntry.content ]
                                        ]
                                , HE.div (HA.class' "contact-options")
                                        [ HE.span (HA.class' { duller: true, invisible: not isClientRender || not model.user.messageTimestamps || not contact.user.messageTimestamps }) <<< SD.ago $ DN.unwrap lastHistoryEntry.date
                                        , HE.div (HA.class' { "unread-messages": true, hidden: numberUnreadMessages == 0 }) <<< HE.span (HA.class' "unread-number") $ show numberUnreadMessages
                                        , HE.div (HA.class' { "duller": true, hidden: numberUnreadMessages > 0 || lastHistoryEntry.sender == contact.user.id || not contact.user.readReceipts || not model.user.readReceipts }) $ show lastHistoryEntry.status
                                        ]
                                ]
                        , HE.hr' (HA.class' "contact-ruler")
                        ]

      -- | Since on mobile contact list takes most of the screen, show a welcoming message for new users
      suggestionsCall =
            HE.div (HA.class' "suggestions-call")
                  [ HE.div (HA.onClick $ ToggleInitialScreen false) backArrow
                  , HE.div (HA.class' { "suggestions-call-middle": true })
                          [ HE.div (HA.class' "welcome-suggestions-call") "Welcome!"
                          , HE.div_ "Tap on either of the arrows to see "
                          , HE.div_ "new users to chat to"
                          ]
                  , HE.div (HA.onClick $ ToggleInitialScreen false) nextArrow
                  ]

      compareLastDate contact anotherContact = compare anotherContact.lastMessageDate contact.lastMessageDate

      countUnread = DF.foldl unread 0
      unread total ss = total + DE.fromEnum (ss.sender /= model.user.id && ss.status < Read)

      chattingId = (_.id <<< _.user) <$> SIC.maybeFindContact model.chatting model.contacts

      -- | Displayed if loading contact from an incoming message fails
      retryLoadingNewContact = SIVR.retry "Failed to sync contacts. You might have missed messages." FetchMissedContacts model.failedRequests

      -- | Displayed if loading contact list fails
      retryLoadingContacts = SIVR.retry "Failed to load contacts" (FetchContacts true) model.failedRequests