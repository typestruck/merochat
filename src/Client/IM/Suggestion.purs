module Client.IM.Suggestion where

import Prelude
import Shared.Types
import Shared.Newtype as SN
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
nextSuggestion model@(IMModel {suggestions, suggesting}) = do
        let nextSuggestion = DM.maybe 0 (_ + 1) suggesting
        if nextSuggestion == DA.length suggestions then
                -- fetch more
                pure model
         else pure <<< SN.updateModel model $ _ {
                 suggesting = Just nextSuggestion,
                 chatting = Nothing
        }
