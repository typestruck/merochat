module Shared.IM.View.Contacts where

import Prelude
import Shared.Types

import Data.Array ((!!))
import Data.Array as DA
import Data.Enum as DE
import Data.Foldable as DF
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Newtype as DN
import Flame (Html)
import Flame.HTML.Attribute as HA
import Flame.HTML.Element as HE
import Shared.Avatar as SA
import Shared.DateTime as SD
import Shared.Markdown as SM

--the ordering of the contact list is only done for the dom nodes
-- model.contacts is left unchanged
contactList :: Boolean -> IMModel -> Html IMMessage
contactList displayLastMessageDates { chatting, contacts, user: { id: userID } } = HE.div [HA.onScroll CheckFetchContacts, HA.class' "contact-list"] <<< DA.mapWithIndex contactEntry $ DA.sortBy compareDateUnread contacts
      where getDate history = do
                  { date: DateTimeWrapper md } <- DA.last history
                  pure md
            compareDateUnread contact anotherContact = compare (getDate anotherContact.history) (getDate contact.history)

            unread total { status, sender } = total + DE.fromEnum (sender /= userID && status == Received)
            countUnread = DF.foldl unread 0
            --should only work for text messages!
            lastMessage = SM.toRestrictedHTML <<< DM.fromMaybe "" <<< map _.content <<< DA.last
            chattingID = do
                  index <- chatting
                  contact <- contacts !! index
                  pure $ contact.user.id

            contactEntry index ({ history, user: { id, name, avatar, headline }}) =
                  let   index' = Just index
                        extraContactClasses = if chattingID == Just id then " chatting-contact" else ""
                        numberUnreadMessages = countUnread history
                  in HE.div [HA.class' $ "contact" <> extraContactClasses, HA.onClick $ ResumeChat id] [
                        HE.div (HA.class' "avatar-contact-list-div") [
                              HE.img [HA.class' $ "avatar-contact-list" <> SA.avatarColorClass index', HA.src $ SA.avatarForRecipient index' avatar]
                        ],
                        HE.div [HA.class' "contact-profile"] [
                              HE.span (HA.class' "contact-name") name,
                             -- HE.br,
                              HE.div' [HA.class' "contact-list-last-message", HA.innerHTML (lastMessage history)]
                        ],
                        HE.div (HA.class' "contact-options") [
                              HE.span (HA.class' { "duller": true, "invisible": not displayLastMessageDates }) <<< HE.text <<< DM.fromMaybe "" <<< map (SD.ago <<< DN.unwrap <<< _.date) $ DA.last history ,
                              HE.div (HA.class' {"unread-messages" :true, "hidden" : numberUnreadMessages == 0}) <<< HE.span (HA.class' "unread-number") $ show numberUnreadMessages
                              -- HE.a (HA.class' "menu-button") $
                              --       HE.svg [HA.class' "i-chevron-bottom svg-16 svg-right", HA.viewBox "0 0 32 32"] $
                              --             HE.path' (HA.d "M30 12 L16 24 2 12"),
                              -- HE.div' (HA.class' "drop-menu fade-in effect")
                        ]
                  ]