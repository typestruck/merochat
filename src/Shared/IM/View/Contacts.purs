module Shared.IM.View.Contacts where

import Prelude
import Shared.Types

import Data.Array ((!!), (:))
import Data.Array as DA
import Data.Enum as DE
import Data.Foldable as DF
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Newtype as DN
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Avatar as SA
import Shared.DateTime as SD
import Shared.IM.View.Profile as SIVP
import Shared.IM.View.Retry as SIVR
import Shared.Markdown as SM

contactList :: Boolean -> IMModel -> Html IMMessage
contactList isClientRender { failedRequests, chatting, contacts, user: { id: userID } } = HE.div [HA.onScroll CheckFetchContacts, HA.class' "contact-list", HA.id "contact-list"] $ retryMissedEvents : DA.snoc allContacts retryFetchContacts
      where --the ordering of the contact list is only done for the dom nodes
            -- model.contacts is left unchanged
            allContacts
                  | DA.null contacts = [suggestionsCall]
                  | otherwise = DA.mapWithIndex contactEntry $ DA.sortBy compareDateUnread contacts

            suggestionsCall = HE.div (HA.class' "suggestions-call") [
                  HE.div (HA.onClick ToggleInitialScreen) $ SIVP.backArrow,
                  HE.div (HA.class' "suggestions-call-middle") [
                        HE.div (HA.class' "welcome-suggestions-call") $ "Welcome!",
                        HE.div_ "Tap on either of the arrows to see ",
                        HE.div_ "your chat suggestions"
                  ],
                  HE.div (HA.onClick ToggleInitialScreen) $ SIVP.nextArrow
            ]

            getDate history = do
                  { date: DateTimeWrapper md } <- DA.last history
                  pure md
            compareDateUnread contact anotherContact = compare (getDate anotherContact.history) (getDate contact.history)

            unread total { status, sender } = total + DE.fromEnum (sender /= userID && status < Read)
            countUnread = DF.foldl unread 0

            lastMessage = DM.fromMaybe "" <<< map _.content
            lastStatus = DM.fromMaybe "" <<< map (show <<< _.status)
            lastDate = DM.fromMaybe "" <<< map (SD.ago <<< DN.unwrap <<< _.date)
            chattingID = do
                  index <- chatting
                  contact <- contacts !! index
                  pure $ contact.user.id

            contactEntry index ({ history, user: { id, name, avatar, headline }}) =
                  let   index' = Just index
                        extraContactClasses = if chattingID == Just id then " chatting-contact" else ""
                        numberUnreadMessages = countUnread history
                        maybeMessage = DA.last history

                  in HE.div [HA.class' $ "contact" <> extraContactClasses, HA.onClick $ ResumeChat id] [
                        HE.div (HA.class' "avatar-contact-list-div") [
                              HE.img [HA.class' $ "avatar-contact-list" <> SA.avatarColorClass index', HA.src $ SA.avatarForRecipient index' avatar]
                        ],
                        HE.div [HA.class' "contact-profile"] [
                              HE.span (HA.class' "contact-name") name,
                              HE.div' [HA.class' "contact-list-last-message", HA.innerHtml <<< SM.parseRestricted $ lastMessage maybeMessage]
                        ],
                        HE.div (HA.class' "contact-options") [
                              HE.span (HA.class' {duller: true, invisible: not isClientRender }) $ lastDate maybeMessage,
                              HE.div (HA.class' { "unread-messages" :true, hidden: numberUnreadMessages == 0}) <<< HE.span (HA.class' "unread-number") $ show numberUnreadMessages,
                              HE.div (HA.class' { duller: true, hidden: numberUnreadMessages > 0 || map _.sender maybeMessage == Just id }) $ lastStatus maybeMessage
                        ]
                  ]

            retryMissedEvents = SIVR.retry "Failed to sync contacts. You might have missed messages." CheckMissedEvents failedRequests
            retryFetchContacts = SIVR.retry "Failed to load contacts" (FetchContacts true) failedRequests
