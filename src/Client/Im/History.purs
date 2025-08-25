module Client.Im.History where

import Client.Im.Flame
import Debug
import Prelude
import Shared.Im.Types

import Client.Common.Network (request)
import Client.Common.Network as CCN
import Client.Im.Scroll as CIS
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Flame as F
import Shared.Im.Contact as SIC
import Shared.Unsafe as SU

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
                                [ CCN.retryableResponse
                                        (FetchHistory contact.user.id true)
                                        (DisplayHistory contact.user.id)
                                        (request.im.history { query: { with: contact.user.id, skip: if contact.shouldFetchChatHistory then 0 else DA.length contact.history } })
                                ]
      | otherwise = F.noMessages model

displayHistory ∷ Int → Array HistoryMessage → ImModel → NoMessages
displayHistory userId history model =
      updatedModel /\
            if shouldFetchChatHistory then
                  [ liftEffect CIS.scrollLastMessage *> pure Nothing ]
            else if not $ DA.null history then
                  [ liftEffect (CIS.scrollIntoView <<< SU.fromJust $ DA.last history) *> pure Nothing ]
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
