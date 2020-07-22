module Client.IM.Suggestion where

import Prelude
import Shared.IM.Types

import Client.Common.Network as CCN
import Client.IM.Flame (NoMessages, MoreMessages, NextMessage)
import Client.IM.Flame as CIF
import Client.IM.Flame as CIF
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Debug.Trace (spy)
import Effect.Aff (Aff)
import Flame ((:>))
import Flame as F
import Shared.Newtype as SN
import Shared.Types (Route(..), JSONResponse(..))

nextSuggestion :: IMModel -> MoreMessages
nextSuggestion model@(IMModel { suggestions, suggesting }) =
      let next = DM.maybe 0 (_ + 1) suggesting
      in      if next == DA.length suggestions then
                  fetchMoreSuggestions model
             else
                  F.noMessages <<< SN.updateModel model $ _ {
                        suggesting = Just next,
                        chatting = Nothing
                  }

previousSuggestion :: IMModel -> MoreMessages
previousSuggestion model@(IMModel { suggesting }) =
      let previous = DM.maybe 0 (_ - 1) suggesting
      in
            if previous < 0 then
                  fetchMoreSuggestions model
             else
                  F.noMessages <<< SN.updateModel model $ _  {
                        suggesting = Just previous,
                        chatting = Nothing
                  }

fetchMoreSuggestions :: IMModel -> NextMessage
fetchMoreSuggestions = (_ :> [Just <<< DisplayMoreSuggestions <$> CCN.get' Suggestions])

displayMoreSuggestions :: Array Suggestion -> IMModel -> NoMessages
displayMoreSuggestions suggestions =
      CIF.diff {
            suggesting: Just 0,
            chatting : Nothing,
            suggestions
      }