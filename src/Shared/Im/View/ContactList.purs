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

import Data.String as DS
import Debug (spy)
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Safe.Coerce as SC
import Shared.Avatar as SA
import Shared.Backer.Contact (backerId)
import Shared.DateTime as SD
import Shared.Im.Contact as SIC
import Shared.Im.Scroll as SIS
import Shared.Im.View.Profile as SIVP
import Shared.Im.View.Retry as SIVR
import Shared.Markdown as SM
import Shared.Modal (Modal(..), ScreenModal(..))
import Shared.Unsafe as SU
import Shared.User (ProfileTab(..), ProfileVisibility(..))

-- | Users that have exchanged messages with the current logged in user
contactList ∷ Boolean → ImModel → Html ImMessage
contactList isClientRender model =
      case model.user.profileVisibility of
            Nobody → HE.div' [ HA.class' "contact-list" ]
            _ →
                  HE.div
                        [ SIS.onScrollEvent CheckFetchContacts
                        , HA.class' "contact-list"
                        ]
                        (retryLoadingNewContact : DA.snoc (DA.snoc contacts retryLoadingContacts) loading)
      where
      -- | Contact list sorting is only done for the dom nodes, model.contacts is left unchanged
      contacts
            | DA.null model.contacts = [ suggestionsCall ]
            | otherwise =
                    let
                          entries = map displayContactListEntry $ DA.sortBy compareLastDate model.contacts
                    in
                          if model.user.temporary then SIVP.signUpCall model.user.joined : entries else entries

      displayContactListEntry contact =
            let
                  numberUnreadMessages = countUnread contact.history
                  lastHistoryEntry = DA.last contact.history
                  backingCall = contact.user.id == backerId
            in
                  HE.div [ HA.class' "contact-wrapper" ]
                        [ HE.div
                                [ HA.class' { contact: true, "chatting-contact": chattingId == Just contact.user.id }
                                , HA.onClick $ if backingCall then SpecialRequest (ToggleModal $ Screen ShowBacker) else ResumeChat contact.user.id
                                ]
                                [ HE.div [ HA.class' "avatar-contact-list-div", HA.title $ if contact.user.onlineStatus && model.user.onlineStatus then show contact.user.availability else "" ]
                                        [ HE.img
                                                ( if contact.user.unseenPosts > 0 then
                                                        [ SA.async
                                                        , SA.decoding "lazy"
                                                        , HA.class' "avatar-contact-list newly-posted"
                                                        , HA.onClick $ ResumeChat contact.user.id
                                                        , HA.onClick $ ToggleShowing contact.user.id ForContacts ShowPosts
                                                        , HA.src $ SA.fromAvatar contact.user
                                                        ]
                                                  else
                                                        [ SA.async
                                                        , SA.decoding "lazy"
                                                        , HA.class' "avatar-contact-list"
                                                        , HA.src $ SA.fromAvatar contact.user
                                                        ]
                                                )
                                        ]
                                , HE.div [ HA.class' "contact-profile" ]
                                        [ HE.div [ HA.class' "contact-online-wrapper" ]
                                                [ HE.span [ HA.class' "contact-name" ] [ HE.text contact.user.name ]
                                                , HE.div' [ HA.class' { "online-indicator": true, hidden: contact.user.availability /= Online || not contact.user.onlineStatus || not model.user.onlineStatus } ]
                                                ]
                                        , if contact.typing && model.user.typingStatus && contact.user.typingStatus then
                                                HE.div [ HA.class' "contact-list-last-message duller typing" ] [ HE.p_ [ HE.text "Typing..." ] ]
                                          else if not $ DS.null contact.draft then
                                                HE.div' [ HA.class' "contact-list-last-message duller message-draft", HA.innerHtml $ SM.parseRestricted ("Draft: " <> contact.draft) ]
                                          else
                                                case lastHistoryEntry of
                                                      Just entry -> HE.div' [ HA.class' "contact-list-last-message duller", HA.innerHtml $ SM.parseRestricted entry.content ]
                                                      _ -> HE.div [ HA.class' "contact-list-last-message duller" ] [HE.i_ [HE.text "No messages yet"]]
                                        ]
                                , HE.div [ HA.class' "contact-options" ]
                                    case lastHistoryEntry of
                                          Just entry ->
                                                [ HE.span [ HA.class' { duller: true, hidden: not isClientRender || not model.user.messageTimestamps || not contact.user.messageTimestamps } ] [ HE.text <<< SD.ago $ SC.coerce entry.date ]
                                                , HE.div [ HA.class' { "unread-messages": true, hidden: numberUnreadMessages == 0 } ] [ HE.span [ HA.class' "unread-number" ] [ HE.text $ show numberUnreadMessages ] ]
                                                , HE.div [ HA.class' { "duller": true, hidden: numberUnreadMessages > 0 || entry.sender == contact.user.id || not contact.user.readReceipts || not model.user.readReceipts } ] [ HE.text $ show entry.status ]
                                                ]
                                          Nothing -> []
                                ]
                        , HE.hr' [ HA.class' "contact-ruler" ]
                        ]

      -- | Since on mobile contact list takes most of the screen, show a welcoming message for new users
      suggestionsCall =
            HE.div [ HA.class' "suggestions-call", HA.onClick $ ToggleInitialScreen false ]
                  [ HE.div [ HA.class' "welcome-suggestions-call" ] [ HE.text "Welcome!" ]
                  , HE.div_ [ HE.text "Tap here to see " ]
                  , HE.div_ [ HE.text "suggested users to chat to" ]
                  ]

      compareLastDate contact anotherContact = compare anotherContact.lastMessageDate contact.lastMessageDate

      countUnread = DF.foldl unread 0
      unread total ss = total + DE.fromEnum (ss.sender /= model.user.id && ss.status < Read)

      chattingId = (_.id <<< _.user) <$> SIC.maybeFindContact model.chatting model.contacts

      -- | Displayed if loading contact from an incoming message fails
      retryLoadingNewContact = SIVR.retry "Failed to sync contacts. You might have missed messages." FetchMissedContacts model.failedRequests

      -- | Displayed if loading contact list fails
      retryLoadingContacts = SIVR.retry "Failed to load contacts" (FetchContacts true) model.failedRequests

      loading = HE.div [ HA.class' "loading-contacts" ] [ HE.div [ HA.class' { "loading": true, hidden: model.freeToFetchContactList } ] [] ]