module Shared.IM.View.Contacts where

import Prelude
import Shared.Types

import Data.Array as DA
import Data.DateTime (DateTime)
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
import Shared.Unsafe as SU

contactList :: Boolean -> IMModel -> Html IMMessage
contactList displayLastMessageDates { chatting, contacts, user: { id: userID } } = HE.div [HA.onScroll CheckFetchContacts, HA.class' "contact-list"] <<< DA.mapWithIndex contactEntry $ DA.sortBy compareDates contacts
      where getDate history = do
                  { date: DateTimeWrapper md } <- DA.last history
                  pure md
            compareDates contact anotherContact = compare (getDate anotherContact.history) (getDate contact.history)

            countUnread total { status, sender } = total + DE.fromEnum (sender /= userID && status == Unread)
            showUnreadCount history' = let count = DF.foldl countUnread 0 history' in if count == 0 then "" else show count
            --should only work for text messages!
            lastMessage = SM.toRestrictedHTML <<< DM.fromMaybe "" <<< map _.content <<< DA.last

            contactEntry index ({ history, user: { id, name, avatar, headline }}) =
                  let   index' = Just index
                        extraContactClasses = if chatting == index' then " chatting-contact" else ""
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
                              HE.span (HA.class' { "duller": true, "invisible": not displayLastMessageDates }) <<< HE.text <<< SD.ago <<< DN.unwrap <<< _.date <<< SU.fromJust $ DA.last history ,
                              HE.text $ showUnreadCount history
                              -- HE.a (HA.class' "menu-button") $
                              --       HE.svg [HA.class' "i-chevron-bottom svg-16 svg-right", HA.viewBox "0 0 32 32"] $
                              --             HE.path' (HA.d "M30 12 L16 24 2 12"),
                              -- HE.div' (HA.class' "drop-menu fade-in effect")
                        ]
                  ]