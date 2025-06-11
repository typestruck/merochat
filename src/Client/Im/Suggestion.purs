module Client.Im.Suggestion where

import Prelude

import Client.Common.Dom as CCD
import Client.Common.Network (request)
import Client.Common.Network as CCN
import Client.Im.Flame (NextMessage, NoMessages, MoreMessages)
import Data.Array ((!!))
import Data.Array as DA
import Data.Enum as DE
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String as DS
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Class as EC
import Effect.Random as ER
import Flame as F
import Safe.Coerce as SC
import Shared.DateTime as SD
import Shared.Element (ElementId(..))
import Shared.Im.Types (ImMessage(..), ImModel, MeroChatCall(..), RetryableRequest(..), ShowChatModal(..), Suggestion, SuggestionsFrom(..))
import Shared.Options.Page (suggestionsPerPage)
import Shared.Unsafe as SU
import Web.DOM.Element as WDE

-- | Display next suggestion card
nextSuggestion ∷ ImModel → MoreMessages
nextSuggestion model =
      if DM.isNothing next then
            fetchMoreSuggestions model
      else
            model
                  { freeToFetchSuggestions = true
                  , suggesting = next
                  , showLargeAvatar = false
                  , showMiniChatInput = false
                  , bugging = Nothing
                  } /\ [ bugUser model ]
      where
      next = moveSuggestion model 1

-- | Display previous suggestion card
previousSuggestion ∷ ImModel → MoreMessages
previousSuggestion model =
      if DM.isNothing previous then
            fetchMoreSuggestions model
      else
            model
                  { freeToFetchSuggestions = true
                  , suggesting = previous
                  , showMiniChatInput = false
                  , showLargeAvatar = false
                  , bugging = Nothing
                  } /\ [ bugUser model ]
      where
      previous = moveSuggestion model (-1)

-- | Set suggesting to the following user id
moveSuggestion ∷ ImModel → Int → Maybe Int
moveSuggestion model by = do
      currentId ← model.suggesting
      index ← DA.findIndex ((_ == currentId) <<< _.id) model.suggestions
      suggestion ← model.suggestions !! (index + by)
      Just $ suggestion.id

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
            , showLargeAvatar = false
            , showMiniChatInput = false
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
            fetchMoreSuggestions model
                  { suggestionsPage = 0
                  , suggestionsFrom = suggestionsFrom
                  }
      else
            model
                  { freeToFetchSuggestions = true
                  , suggesting = _.id <$> DA.head suggestions
                  , suggestions = suggestions
                  , suggestionsPage = if suggestionsSize == 0 || suggestionsFrom /= model.suggestionsFrom then 0 else model.suggestionsPage + 1
                  , suggestionsFrom = suggestionsFrom
                  } /\ [ scrollToTop, track ]
      where
      suggestionsSize = DA.length suggestions

      lowQualityUsersBin = 5
      lowQualityUsersIn = DA.length <<< DA.filter ((_ >= lowQualityUsersBin) <<< _.bin)
      suggestionsFrom
            | model.suggestionsFrom /= OnlineOnly && (suggestionsSize == 0 || lowQualityUsersIn suggestions / suggestionsSize * 100 >= 60) = DM.fromMaybe ThisWeek $ DE.succ model.suggestionsFrom
            | otherwise = model.suggestionsFrom

      scrollToTop = do
            EC.liftEffect (CCD.unsafeGetElementById Cards >>= WDE.setScrollTop 0.0)
            pure Nothing
      track = pure $ Just TrackAvailability

-- | Show or hide full user profile
toggleSuggestionChatInput ∷ Int → ImModel → NoMessages
toggleSuggestionChatInput id model = F.noMessages model
      { showSuggestionChatInput = Just id
      , suggesting = Just id
      }

toggleContactProfile ∷ ImModel → NoMessages
toggleContactProfile model = F.noMessages model
      { fullContactProfileVisible = not model.fullContactProfileVisible
      }

toggleCollapsedMiniSuggestions ∷ ImModel → NoMessages
toggleCollapsedMiniSuggestions model = F.noMessages model
      { showCollapsedMiniSuggestions = not model.showCollapsedMiniSuggestions
      }

toggleLargeAvatar ∷ ImModel → NoMessages
toggleLargeAvatar model = model
      { showLargeAvatar = not model.showLargeAvatar
      } /\ []

-- | Show suggestion cards again
resumeSuggesting ∷ ImModel → NoMessages
resumeSuggesting model =
      model
            { chatting = Nothing
            , toggleChatModal = HideChatModal
            , showSuggestionChatInput = Nothing
            , editing = Nothing
            } /\ [ updateDraft ]
      where
      updateDraft = EC.liftEffect do
            input ← CCD.unsafeGetElementById ChatInput
            draft ← CCD.value input
            CCD.setValue input ""
            if DM.isNothing model.chatting then
                  pure Nothing
            else
                  pure <<< Just $ UpdateDraft (SU.fromJust model.chatting) draft

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
      }
