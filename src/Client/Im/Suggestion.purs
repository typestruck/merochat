module Client.Im.Suggestion where

import Prelude
import Shared.Im.Types

import Client.Dom as CCD
import Client.Im.Flame (MoreMessages, NextMessage, NoMessages)
import Client.Network (routes)
import Client.Network as CCN
import Client.Network as CCNT
import Data.Array ((!!))
import Data.Array as DA
import Data.Enum as DE
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Tuple.Nested ((/\))
import Effect.Class as EC
import Flame as F
import Shared.Availability (Availability(..))
import Shared.Backer.Contact (backerId, backerUser)
import Shared.DateTime (DateTimeWrapper(..))
import Shared.Element (ElementId(..))
import Shared.Im.Contact as SIC
import Shared.Modal (Modal(..))
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
                  } /\ []
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
                  } /\ []
      where
      previous = moveSuggestion model (-1)

-- | Set suggesting to the following user id
moveSuggestion ∷ ImModel → Int → Maybe Int
moveSuggestion model by = do
      currentId ← model.suggesting
      index ← DA.findIndex ((_ == currentId) <<< _.id) model.suggestions
      suggestion ← model.suggestions !! (index + by)
      Just $ suggestion.id

-- | Fetch next page of suggestions
fetchMoreSuggestions ∷ ImModel → NextMessage
fetchMoreSuggestions model =
      model
            { freeToFetchSuggestions = false --ui uses this flag to show a loading icon and prevent repeated requests
            , failedRequests = []
            , showLargeAvatar = false
            , showMiniChatInput = false
            } /\
            [ CCN.retryableRequest NextSuggestion DisplayMoreSuggestions $ routes.im.suggestions
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
                  , suggestions = suggestions <> DA.filter ((backerId == _) <<< _.id) model.suggestions
                  , suggestionsPage = if suggestionsSize == 0 || suggestionsFrom /= model.suggestionsFrom then 0 else model.suggestionsPage + 1
                  , suggestionsFrom = suggestionsFrom
                  } /\ [ scrollToTop, track ]
      where
      suggestionsSize = DA.length suggestions

      lowQualityUsersBin = 5
      lowQualityUsersIn = DA.length <<< DA.filter ((_ >= lowQualityUsersBin) <<< _.bin)
      suggestionsFrom
            | model.suggestionsFrom /= OnlineOnly && model.suggestionsFrom /= FromContacts && (suggestionsSize == 0 || lowQualityUsersIn suggestions / suggestionsSize * 100 >= 60) = DM.fromMaybe ThisWeek $ DE.succ model.suggestionsFrom
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
toggleLargeAvatar model =
      model
            { showLargeAvatar = not model.showLargeAvatar
            } /\ []

-- | Show suggestion cards again
resumeSuggesting ∷ ImModel → NoMessages
resumeSuggesting model =
      model
            { chatting = Nothing
            , modal = HideModal
            , showSuggestionChatInput = Nothing
            , suggestions = byAvailability model.suggestions
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

--sort the suggestion when the user is not looking
byAvailability ∷ Array Suggestion → Array Suggestion
byAvailability suggestions = DA.snoc (DA.filter ((backerId /= _) <<< _.id) $ DA.sortBy available suggestions) backerUser
      where
      available suggestion anotherSuggestion =
            case suggestion.availability, anotherSuggestion.availability of
                  LastSeen (DateTimeWrapper dt), LastSeen (DateTimeWrapper anotherDt) → anotherDt `compare` dt
                  a, s → s `compare` a

toggleSuggestionsFrom ∷ SuggestionsFrom -> ImModel → MoreMessages
toggleSuggestionsFrom from model = fetchMoreSuggestions model
      { suggestionsFrom = from
      , suggestionsPage = 0
      , toggleContextMenu = HideContextMenu
      }

resumeSuggestionChat ∷ Int → ImModel → NextMessage
resumeSuggestionChat userId model =
      model { loadingContact = Just userId } /\ [ resume ]
      where
      existing = SIC.findContact userId model.contacts
      resume = case existing of
            Nothing → CCNT.retryableRequest (FetchContacts true) (DisplaySuggestionContact userId) $ routes.im.contact { query: { id: userId } }
            _ → pure <<< Just $ ResumeChat userId

favorite :: Int → ImModel → NoMessages
favorite userId model = model /\ [ fav ]
      where fav = do
                  void <<< CCNT.silentRequest $ routes.im.favorite { body : { userId }}
                  pure Nothing