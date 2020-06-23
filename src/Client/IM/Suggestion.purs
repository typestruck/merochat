module Client.IM.Suggestion where

import Prelude
import Shared.IM.Types

import Client.Common.Network as CCN
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Debug.Trace (spy)
import Shared.Types(Route(..), JSONResponse(..))
import Effect.Aff (Aff)
import Flame.Application.Effectful (AffUpdate)
import Flame.Application.Effectful as FAE
import Shared.Newtype as SN

update :: AffUpdate IMModel SuggestionMessage
update { model, message } =
        case message of
                PreviousSuggestion -> previousSuggestion model
                NextSuggestion -> nextSuggestion model

nextSuggestion :: IMModel -> Aff (IMModel -> IMModel)
nextSuggestion model@(IMModel { suggestions, suggesting }) = do
        let next = DM.maybe 0 (_ + 1) suggesting
        if next == DA.length suggestions then
                fetchMoreSuggestions
         else
                FAE.diff {
                        suggesting: Just next,
                        chatting : Nothing
                }

previousSuggestion :: IMModel -> Aff (IMModel -> IMModel)
previousSuggestion model@(IMModel { suggesting }) = do
        let previous = DM.maybe 0 (_ - 1) suggesting
        if previous < 0 then
                fetchMoreSuggestions
         else
                FAE.diff {
                        suggesting: Just previous,
                        chatting : Nothing
                }

fetchMoreSuggestions :: Aff (IMModel -> IMModel)
fetchMoreSuggestions = do
        JSONResponse suggestions <- CCN.get' Suggestions
        FAE.diff {
                suggesting: Just 0,
                chatting : Nothing,
                suggestions
        }