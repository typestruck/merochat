module Client.Im.History where

import Prelude

import Client.Ui.Document as CUD
import Client.Ui.Element as CUE
import Client.Im.Flame (MoreMessages, NoMessages)
import Client.Im.Scroll as CIS
import Client.Network (routes)
import Client.Network as CN
import Client.Ui.Event as CUEV
import Data.Array as DA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String as DS
import Data.Tuple.Nested ((/\))
import Effect.Class as EC
import Flame as F
import Flame.Html.Event (Event)
import Shared.Im.Contact as SIC
import Shared.Im.Types (HistoryMessage, ImMessage(..), ImModel, ReactWith(..), RetryableRequest(..), ShowContextMenu(..))
import Shared.Unsafe as SU
import Unsafe.Coerce as UC
import Web.HTML.HTMLElement as WHHE

--to avoid issues with older missed unread messages just get the whole chat history on first load
fetchHistory ∷ Int → Boolean → ImModel → MoreMessages
fetchHistory userId shouldFetch model
      | model.freeToFetchChatHistory && shouldFetch =
              case SIC.findContact userId model.contacts of
                    Nothing → F.noMessages model
                    Just contact →
                          model
                                { freeToFetchChatHistory = false
                                } /\
                                [ CN.retryableRequest
                                        (FetchHistory contact.user.id true)
                                        (DisplayHistory contact.user.id)
                                        (routes.im.history { query: { with: contact.user.id, skip: if contact.shouldFetchChatHistory then 0 else DA.length contact.history } })
                                ]
      | otherwise = F.noMessages model

displayHistory ∷ Int → Array HistoryMessage → ImModel → NoMessages
displayHistory userId history model =
      updatedModel /\
            if shouldFetchChatHistory then
                  [ EC.liftEffect CIS.scrollLastMessage *> pure Nothing ]
            else if not $ DA.null history then
                  [ EC.liftEffect (CIS.scrollIntoView <<< SU.fromJust $ DA.last history) *> pure Nothing ]
            else
                  []
      where
      shouldFetchChatHistory = DM.maybe false _.shouldFetchChatHistory $ SIC.findContact userId model.contacts

      updateContact contact
            | contact.user.id == userId = updateHistory contact
            | otherwise = contact

      --when the chat history is first loaded (either by selecting the contact or sending a message from suggestions)
      -- avoid overwritting the whole conversation or duplicating unread messages
      updateHistory contact = contact
            { shouldFetchChatHistory = false
            , history = fixHistory $ contact.history <> history
            }

      updatedModel = model
            { freeToFetchChatHistory = true
            , contacts = map updateContact model.contacts
            }

fixHistory ∷ Array HistoryMessage → Array HistoryMessage
fixHistory = DA.sortWith _.date <<< DA.nubByEq dedup
      where
      dedup entry anotherEntry = entry.id == anotherEntry.id

setReacWithText ∷ String → Event → ImModel → NoMessages
setReacWithText id event model = model { react = WithText } /\ [ stop ]
      where
      stop = EC.liftEffect do
            CUEV.stopPropagation event
            element ← CUD.unsafeQuerySelector $ "#" <> id
            WHHE.focus $ UC.unsafeCoerce element
            pure Nothing

react ∷ Int → Int → Either String String → Event → ImModel → NoMessages
react userId messageId value event model = model /\ [ save ]
      where
      save = do
            EC.liftEffect $ CUEV.stopPropagation event
            reaction ← case value of
                  Right text → pure text
                  Left id → EC.liftEffect (CUD.unsafeQuerySelector $ "#" <> id) >>= CUE.value >>= (pure <<< DS.toUpper)
            void <<< CN.silentRequest $ routes.im.react { body: { id: messageId, reaction } }
            pure <<< Just $ DisplayReaction userId messageId reaction

displayReaction ∷ Int → Int → String → ImModel → NoMessages
displayReaction userId messageId reaction model = model { react = WithEmoji, toggleContextMenu = HideContextMenu, contacts = map updateContact model.contacts } /\ []
      where
      updateHistory message
            | message.id == messageId = message { reaction = Just reaction }
            | otherwise = message
      updateContact contact
            | contact.user.id == userId = contact { history = map updateHistory contact.history }
            | otherwise = contact
