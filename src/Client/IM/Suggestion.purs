module Client.IM.Suggestion where

import Prelude
import Shared.Types

import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Effect.Aff (Aff)
import Flame (World)

update :: World IMModel IMMessage -> IMModel -> SuggestionMessage -> Aff IMModel
update _ model =
        case _ of
                NextSuggestion -> nextSuggestion model

nextSuggestion :: IMModel -> Aff IMModel
nextSuggestion model@(IMModel m@{suggestions, chatting}) = do
        let suggestionsSize = DA.length suggestions

        if suggestionsSize == 0 || DM.maybe 0 (_ + 1) chatting > suggestionsSize then
                -- fetch more
                pure model
         else pure <<< IMModel $ m { chatting = DM.maybe (Just 0) (Just <<< (_ + 1)) chatting }
