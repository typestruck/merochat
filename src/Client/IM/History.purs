module Client.IM.History where

import Client.IM.Flame
import Prelude
import Shared.Types

import Client.Common.DOM as CCD
import Client.Common.Network (request)
import Client.Common.Network as CCN
import Client.IM.Flame as CIF
import Client.IM.Scroll as CIS
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Flame ((:>))
import Flame as F
import Shared.IM.Contact as SIC
import Shared.Unsafe as SU
import Web.DOM.Element as WDE

checkFetchHistory :: IMModel -> MoreMessages
checkFetchHistory model@{ freeToFetchChatHistory }
      | freeToFetchChatHistory = model :> [ Just <<< SpecialRequest <<< FetchHistory <$> getScrollTop ]

      where getScrollTop = liftEffect do
                  element <- CCD.unsafeQuerySelector "#message-history"
                  (_ < 1.0) <$> WDE.scrollTop element

      | otherwise = F.noMessages model

fetchHistory :: Boolean -> IMModel -> MoreMessages
fetchHistory shouldFetch model@{ chatting, contacts }
      | shouldFetch =
            let { history, user: { id } } = SIC.chattingContact contacts chatting
            in model {
                  freeToFetchChatHistory = false
            } :> [ CCN.retryableResponse (FetchHistory true) DisplayHistory (request.im.history { query: { with: id, skip: DA.length history } })   ]
      | otherwise = F.noMessages model

displayHistory :: Array HistoryMessage -> IMModel -> NoMessages
displayHistory chatHistory model@{ chatting, contacts } =
      let   contact@{ history, shouldFetchChatHistory } = SIC.chattingContact contacts chatting
            updatedModel = model {
                  freeToFetchChatHistory = true,
                  contacts = SU.fromJust do
                        index <- chatting
                        let contact' = contact {
                              shouldFetchChatHistory = false,
                              history = chatHistory <> history
                        }
                        DA.updateAt index contact' contacts
            }
      in
            if shouldFetchChatHistory then
                  CIF.nothingNext updatedModel $ liftEffect CIS.scrollLastMessage
            else
                  F.noMessages updatedModel