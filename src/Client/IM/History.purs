module Client.IM.History where

import Client.IM.Flame
import Prelude
import Shared.IM.Types
import Shared.Types

import Client.Common.DOM as CCD
import Client.Common.Network as CCN
import Data.Array ((!!))
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Newtype as DN
import Debug.Trace (spy)
import Effect.Class (liftEffect)
import Flame ((:>))
import Flame as F
import Shared.Newtype as SN
import Shared.Unsafe as SU
import Web.DOM.Element as WDE

update :: IMModel -> ChatHistoryMessage -> MoreMessages
update model = case _ of
      CheckScrollTop -> checkScrollTop model
      FetchHistory shouldFetch -> fetchHistory shouldFetch model
      DisplayHistory (JSONResponse history) -> displayHistory history model

checkScrollTop :: IMModel -> MoreMessages
checkScrollTop model@(IMModel { freeToFetchChatHistory })
      | freeToFetchChatHistory = model :> [ Just <<< HM <<< FetchHistory <$> getScrollTop ]

      where getScrollTop = liftEffect do
                  element <- CCD.querySelector "#message-history-wrapper"
                  (_ < 1.0) <$> WDE.scrollTop element

      | otherwise = F.noMessages model

fetchHistory :: Boolean -> IMModel -> MoreMessages
fetchHistory shouldFetch model@(IMModel { chatting, contacts })
      | shouldFetch =
            let Contact { history, user: IMUser { id } } = SU.fromJust "fetchHistory" do
                  index <- chatting
                  contacts !! index
            in (SN.updateModel model $ _ {
                  freeToFetchChatHistory = false
            }) :> [ Just <<< HM <<< DisplayHistory <$> CCN.get' (History {
                        skip: DA.length history,
                        with: id
                  })
            ]
      | otherwise = F.noMessages model

displayHistory :: Array HistoryMessage -> IMModel -> NoMessages
displayHistory chatHistory model@(IMModel { chatting, contacts }) = F.noMessages <<< SN.updateModel model $ _ {
      freeToFetchChatHistory = true,
      contacts = SU.fromJust "displayHistory" do
            index <- chatting
            contact@(Contact { history }) <- contacts !! index
            let contact' = SN.updateContact contact $ _ {
                  history = chatHistory <> history
            }
            DA.updateAt index contact' contacts
}