module Client.IM.History where

import Client.IM.Flame
import Prelude
import Shared.IM.Types

import Client.Common.DOM as CCD
import Client.Common.Network as CCN
import Data.Maybe as DM
import Effect.Class (liftEffect)
import Flame ((:>))
import Flame as F
import Shared.Newtype as SN
import Web.DOM.Element as WDE

update :: IMModel -> ChatHistoryMessage -> MoreMessages
update model = case _ of
      CheckScrollTop -> checkScrollTop model
      FetchHistory shouldFetch -> fetchHistory shouldFetch model
      DisplayHistory history -> displayHistory history model

checkScrollTop model@(IMModel { freeToFetchChatHistory })
      | freeToFetchChatHistory = model :> [ HM FetchHistory <$> getScrollTop ]

      where getScrollTop = liftEffect do
                  element <- CCD.querySelector "message-history"
                  (_ == 0) <$> WDE.scrollTop element

      | otherwise = F.noMessages model

fetchHistory shouldFetch model
      | shouldFetch = SN.updateModel model _ $ {
            freeToFetchChatHistory = false
      } :> [ HM DisplayHistory <$> CCN.get' History ]
      | otherwise = F.noMessages model

displayHistory history model = F.noMessages <<< SN.updateModel model $ _ {
      freeToFetchChatHistory = true,
      contacts =
}
