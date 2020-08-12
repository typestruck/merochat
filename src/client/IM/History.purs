module Client.IM.History where

import Client.IM.Flame
import Prelude
import Shared.IM.Types
import Shared.Types

import Client.Common.DOM as CCD
import Client.Common.Network as CCN
import Client.IM.Flame as CIF
import Client.IM.Scroll as CIS
import Data.Array ((!!))
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Newtype as DN
import Debug.Trace (spy)
import Effect.Class (liftEffect)
import Flame ((:>))
import Flame as F
import Shared.IM.Contact as SIC
import Shared.Newtype as SN
import Shared.Unsafe as SU
import Web.DOM.Element as WDE

checkFetchHistory :: IMModel -> MoreMessages
checkFetchHistory model@(IMModel { freeToFetchChatHistory })
      | freeToFetchChatHistory = model :> [ Just <<< FetchHistory <$> getScrollTop ]

      where getScrollTop = liftEffect do
                  element <- CCD.querySelector "#message-history-wrapper"
                  (_ < 1.0) <$> WDE.scrollTop element

      | otherwise = F.noMessages model

fetchHistory :: Boolean -> IMModel -> MoreMessages
fetchHistory shouldFetch model@(IMModel { chatting, contacts })
      | shouldFetch =
            let Contact { history, user: IMUser { id } } = SIC.chattingContact contacts chatting
            in (SN.updateModel model $ _ {
                  freeToFetchChatHistory = false
            }) :> [ Just <<< DisplayHistory <$> CCN.get' (History {
                        skip: DA.length history,
                        with: id
                  })
            ]
      | otherwise = F.noMessages model

displayHistory :: Array HistoryMessage -> IMModel -> NoMessages
displayHistory chatHistory model@(IMModel { chatting, contacts }) =
      let   contact@(Contact { history, shouldFetchChatHistory }) = SIC.chattingContact contacts chatting
            updatedModel = SN.updateModel model $ _ {
                  freeToFetchChatHistory = true,
                  contacts = SU.fromJust do
                        index <- chatting
                        let contact' = SN.updateContact contact $ _ {
                              history = chatHistory <> history
                        }
                        DA.updateAt index contact' contacts
            }
      in
            if shouldFetchChatHistory then
                  CIF.nothingNext updatedModel $ liftEffect CIS.scrollLastMessage
             else
                  F.noMessages updatedModel