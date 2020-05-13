module Client.IM.Suggestion where

import Prelude
import Shared.IM.Types

import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Effect.Aff (Aff)
import Flame.Application.Effectful (Environment)
import Shared.Newtype as SN

update :: Environment IMModel SuggestionMessage -> Aff IMModel
update { model, message } =
        case message of
                NextSuggestion -> nextSuggestion model

nextSuggestion :: IMModel -> Aff IMModel
nextSuggestion model@(IMModel {suggestions, suggesting}) = do
        let next = DM.maybe 0 (_ + 1) suggesting
        if next == DA.length suggestions then
                -- fetch more
                pure model
         else pure <<< SN.updateModel model $ _ {
                 suggesting = Just next,
                 chatting = Nothing
        }
