module Client.Im.History where

import Client.Im.Flame
import Prelude
import Shared.Im.Types

import Client.Common.Dom as CCD
import Client.Common.Network (request)
import Client.Common.Network as CCN
import Client.Im.Flame as CIF
import Client.Im.Scroll as CIS
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Effect.Class (liftEffect)
import Flame ((:>))
import Flame as F
import Shared.Element (ElementId(..))
import Shared.Im.Contact as SIC
import Shared.Unsafe as SU
import Web.DOM.Element as WDE

checkFetchHistory ∷ ImModel → MoreMessages
checkFetchHistory model@{ freeToFetchChatHistory }
      | freeToFetchChatHistory = model :> [ Just <<< SpecialRequest <<< FetchHistory <$> getScrollTop ]

              where
              getScrollTop = liftEffect do
                    element ← CCD.unsafeGetElementById MessageHistory
                    (_ < 1.0) <$> WDE.scrollTop element

      | otherwise = F.noMessages model

--to avoid issues with older missed unread messages just get the whole chat history on first load
fetchHistory ∷ Boolean → ImModel → MoreMessages
fetchHistory shouldFetch model@{ chatting, contacts, experimenting }
      | shouldFetch =
              let
                    { history, shouldFetchChatHistory, user: { id }, impersonating } = SIC.chattingContact contacts chatting
              in
                    if DM.isJust experimenting || DM.isJust impersonating then --impersonated messages are not saved in the database

                          displayHistory [] model
                    else
                          model
                                { freeToFetchChatHistory = false
                                } :> [ CCN.retryableResponse (FetchHistory true) DisplayHistory (request.im.history { query: { with: id, skip: if shouldFetchChatHistory then 0 else DA.length history } }) ]
      | otherwise = F.noMessages model

displayHistory ∷ Array HistoryMessage → ImModel → NoMessages
displayHistory chatHistory model@{ chatting, contacts } =
      let
            contact@{ history, shouldFetchChatHistory } = SIC.chattingContact contacts chatting
            updatedModel = model
                  { freeToFetchChatHistory = true
                  , contacts = SU.fromJust do
                          index ← chatting
                          let
                                contact' = contact
                                      { shouldFetchChatHistory = false
                                      , history = if shouldFetchChatHistory then chatHistory else (chatHistory <> history) --see fetchHistory
                                      }
                          DA.updateAt index contact' contacts
                  }
      in
            if shouldFetchChatHistory then
                  CIF.nothingNext updatedModel $ liftEffect CIS.scrollLastMessage
            else if not $ DA.null chatHistory then
                  CIF.nothingNext updatedModel <<< liftEffect <<< CIS.scrollIntoView <<< SU.fromJust $ DA.last chatHistory
            else
                  F.noMessages updatedModel