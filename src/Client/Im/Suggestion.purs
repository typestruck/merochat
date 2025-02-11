module Client.Im.Suggestion where

import Prelude
import Shared.Im.Types (ImMessage(..), ImModel, MeroChatCall(..), RetryableRequest(..), ShowChatModal(..), Suggestion, SuggestionsFrom(..))

import Client.Common.Network (request)
import Client.Common.Network as CCN
import Client.Im.Flame (NextMessage, NoMessages, MoreMessages)
import Data.Array as DA
import Data.Enum as DE
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Class as EC
import Effect.Random as ER
import Flame as F
import Safe.Coerce as SC
import Shared.DateTime as SD
import Shared.Options.Page (suggestionsPerPage)

-- | Display next suggestion card
nextSuggestion ∷ ImModel → MoreMessages
nextSuggestion model =
      if next >= DA.length model.suggestions then
            fetchMoreSuggestions model
      else
            model
                  { freeToFetchSuggestions = true
                  , suggesting = next
                  , chatting = Nothing
                  , bugging = Nothing
                  } /\ [ bugUser model ]
      where
      next = model.suggesting + 1

-- | Display previous suggestion card
previousSuggestion ∷ ImModel → MoreMessages
previousSuggestion model =
      if previous < 0 then
            fetchMoreSuggestions model
      else
            model
                  { freeToFetchSuggestions = true
                  , suggesting = previous
                  , chatting = Nothing
                  , bugging = Nothing
                  } /\ [ bugUser model ]
      where
      previous = model.suggesting - 1

-- | When moving suggestion cards, diplay a special card n% of the time
bugUser ∷ ImModel → Aff (Maybe ImMessage)
bugUser model = do
      chance ← EC.liftEffect $ ER.randomInt 0 100
      --bug user only if account is older than 3 days
      if chance <= 10 && SD.daysDiff (SC.coerce model.user.joined) > 3 then
            pure <<< Just $ SetBugging Backing
      else
            pure Nothing

-- | Fetch next page of suggestions
fetchMoreSuggestions ∷ ImModel → NextMessage
fetchMoreSuggestions model =
      model
            { freeToFetchSuggestions = false --ui uses this flag to show a loading icon and prevent repeated requests
            , failedRequests = []
            , bugging = Nothing
            } /\
            [ CCN.retryableResponse NextSuggestion DisplayMoreSuggestions $ request.im.suggestions
                    { query:
                            { skip: suggestionsPerPage * model.suggestionsPage
                            , sg: model.suggestionsFrom
                            }
                    }
            ]

-- | Show these suggesions to the user
-- |
-- | Suggestions are picked according to `SuggestionsFrom`. If 60% of the suggestions are low quality users, switch to next option in `SuggestionsFrom``
displayMoreSuggestions ∷ Array Suggestion → ImModel → MoreMessages
displayMoreSuggestions suggestions model =
      if suggestionsSize == 0 && model.suggestionsPage > 0 then
            fetchMoreSuggestions $ model
                  { suggestionsPage = 0
                  , suggesting = suggesting
                  , suggestionsFrom = suggestionsFrom
                  }
      else
            F.noMessages model
                  { suggesting = suggesting
                  , chatting = Nothing
                  , freeToFetchSuggestions = true
                  , suggestions = suggestions
                  , suggestionsPage = if suggestionsSize == 0 || suggestionsFrom /= model.suggestionsFrom then 0 else model.suggestionsPage + 1
                  , suggestionsFrom = suggestionsFrom
                  }
      where
      suggestionsSize = DA.length suggestions
      suggesting = if suggestionsSize <= 1 then 0 else 1

      lowQualityUsersBin = 5
      lowQualityUsersIn = DA.length <<< DA.filter ((_ >= lowQualityUsersBin) <<< _.bin)
      suggestionsFrom
            | model.suggestionsFrom /= OnlineOnly && (suggestionsSize == 0 || lowQualityUsersIn suggestions / suggestionsSize * 100 >= 60) = DM.fromMaybe ThisWeek $ DE.succ model.suggestionsFrom
            | otherwise = model.suggestionsFrom

-- | Show or hide full user profile
toggleContactProfile ∷ ImModel → NoMessages
toggleContactProfile model = F.noMessages model
      { fullContactProfileVisible = not model.fullContactProfileVisible
      }

-- | Show suggestion cards again
resumeSuggesting ∷ ImModel → NoMessages
resumeSuggesting model = F.noMessages model
      { chatting = Nothing
      , toggleChatModal = HideChatModal
      , editing = Nothing
      }

-- | Switch to on or from online only suggestions
toggleSuggestionsFromOnline ∷ ImModel → MoreMessages
toggleSuggestionsFromOnline model = fetchMoreSuggestions model
      { suggestionsFrom = if model.suggestionsFrom == OnlineOnly then ThisWeek else OnlineOnly
      , suggestionsPage = 0
      }

-- | Display special card instead of suggestion
setBugging ∷ MeroChatCall → ImModel → NoMessages
setBugging mc model = F.noMessages model
      { bugging = Just mc
      --offset index to account for non profile suggestion
      , suggesting = if model.suggesting > 0 then model.suggesting - 1 else model.suggesting
      }
