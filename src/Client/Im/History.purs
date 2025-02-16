module Client.Im.History where

import Client.Im.Flame
import Debug
import Prelude
import Shared.Im.Types

import Client.Common.Dom as CCD
import Client.Common.Network (request)
import Client.Common.Network as CCN
import Client.Im.Scroll as CIS
import Control.Alt ((<|>))
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Flame as F
import Shared.Element (ElementId(..))
import Shared.Im.Contact as SIC
import Shared.Unsafe as SU
import Web.DOM.Element as WDE

checkFetchHistory ∷ Int → ImModel → MoreMessages
checkFetchHistory userId model =
      model /\
            if model.freeToFetchChatHistory then [ Just <<< SpecialRequest <<< (FetchHistory userId) <$> getScrollTop ] else []
      where
      getScrollTop = liftEffect do
            element ← CCD.unsafeGetElementById MessageHistory
            (_ < 1.0) <$> WDE.scrollTop element

--to avoid issues with older missed unread messages just get the whole chat history on first load
fetchHistory ∷ Int → Boolean → ImModel → MoreMessages
fetchHistory userId shouldFetch model
      | shouldFetch =
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

      --when the chat history is first loaded (either by selecting hte contact or sending a message from suggesitons)
      -- avoid overwritting the whole conversation or duplicating unread messages
      dedup entry anotherEntry = entry.id == anotherEntry.id
      updateHistory contact = contact
            { shouldFetchChatHistory = false
            , history = if shouldFetchChatHistory then DA.nubByEq dedup (history <> contact.history) else history <> contact.history --see fetchHistory
            }

      updatedModel = model
            { freeToFetchChatHistory = true
            , contacts = map updateContact model.contacts
            }

