module Client.Im.Suggestion where

import Prelude
import Shared.Experiments.Types
import Shared.Im.Types

import Client.Common.Network (request)
import Client.Common.Network as CCN
import Client.Im.Flame (NextMessage, NoMessages, MoreMessages)
import Client.Im.WebSocket as CIW
import Data.Array ((:))
import Data.Array as DA
import Data.Either (Either(..))
import Data.Enum as DE
import Data.Maybe (Maybe(..))
import Data.Maybe as DM

import Data.Tuple.Nested ((/\))
import Debug (spy)
import Effect.Class (liftEffect)
import Effect.Random as ER
import Flame as F
import Shared.Options.Page (suggestionsPerPage)
import Web.Socket.WebSocket (WebSocket)

nextSuggestion ∷ ImModel → MoreMessages
nextSuggestion model =
      if next >= DA.length model.suggestions then
            fetchMoreSuggestions model
      else
            model
                  { freeToFetchSuggestions = true
                  , suggesting = Just next
                  , chatting = Nothing
                  } /\ [ bugUser ]
      where
      next = DM.maybe 0 (_ + 1) model.suggesting
      bugUser = do
            chance ← liftEffect $ ER.randomInt 0 100
           {- if chance <= 2 then
                  pure <<< Just $ SetBugging Experimenting
            else -}
            if chance <= 10 then
                  pure <<< Just $ SetBugging Backing
            else
                  pure Nothing

previousSuggestion ∷ ImModel → MoreMessages
previousSuggestion model@{ suggesting } =
      let
            previous = DM.maybe 0 (_ - 1) suggesting
      in
            if previous < 0 then
                  fetchMoreSuggestions model
            else
                  F.noMessages $ model
                        { freeToFetchSuggestions = true
                        , suggesting = Just previous
                        , chatting = Nothing
                        }

fetchMoreSuggestions ∷ ImModel → NextMessage
fetchMoreSuggestions model =
      model
            { freeToFetchSuggestions = false
            , failedRequests = []
            } /\
            [ CCN.retryableResponse NextSuggestion DisplayMoreSuggestions $ request.im.suggestions
                    { query:
                            { skip: suggestionsPerPage * model.suggestionsPage
                            , sg: model.suggestionsFrom
                            }
                    }
            ]

displayMoreSuggestions ∷ Array Suggestion → ImModel → MoreMessages
displayMoreSuggestions suggestions model =
      --if we looped through all the suggestions, retry
      if suggestionsSize == 0 && model.suggestionsPage > 0 then
            fetchMoreSuggestions $ model
                  { suggestionsPage = 0
                  , suggesting = suggesting
                  , suggestionsFrom = sg
                  }
      else
            F.noMessages $ model
                  { suggesting = suggesting
                  , chatting = Nothing
                  , freeToFetchSuggestions = true
                  , suggestions = suggestions
                  , suggestionsPage = if suggestionsSize == 0 || sg /= model.suggestionsFrom then 0 else model.suggestionsPage + 1
                  , suggestionsFrom = sg
                  }
      where
      suggestionsSize = DA.length suggestions
      suggesting = Just $ if suggestionsSize <= 1 then 0 else 1
      shouldSwithCategory = model.suggestionsFrom /= OnlineOnly && (suggestionsSize == 0 || (DA.length $ DA.filter ((_ > 4) <<< _.bin) suggestions) / DA.length suggestions * 100 >= 60)
      sg
            | shouldSwithCategory = DM.fromMaybe ThisWeek $ DE.succ model.suggestionsFrom
            | otherwise = model.suggestionsFrom

blockUser ∷ WebSocket → Int → ImModel → NextMessage
blockUser webSocket id model =
      updateAfterBlock id model /\
            [ do
                    result ← CCN.defaultResponse $ request.im.block { body: { id } }
                    case result of
                          Left _ → pure <<< Just $ RequestFailed { request: BlockUser id, errorMessage: Nothing }
                          _ → do
                                liftEffect <<< CIW.sendPayload webSocket $ UnavailableFor { id }
                                pure Nothing
            ]

updateAfterBlock ∷ Int → ImModel → ImModel
updateAfterBlock blocked model@{ contacts, suggestions, blockedUsers } =
      model
            { contacts = DA.filter ((blocked /= _) <<< fromContact) contacts
            , suggestions = DA.filter ((blocked /= _) <<< fromUser) suggestions
            , blockedUsers = blocked : blockedUsers
            , chatting = Nothing
            , failedRequests = []
            , initialScreen = true
            , toggleModal = HideUserMenuModal
            , toggleContextMenu = HideContextMenu
            }
      where
      fromContact { user } = fromUser user
      fromUser { id } = id

toggleContactProfile ∷ ImModel → NoMessages
toggleContactProfile model@{ fullContactProfileVisible } = F.noMessages $ model
      { fullContactProfileVisible = not fullContactProfileVisible
      }

resumeSuggesting ∷ ImModel → NoMessages
resumeSuggesting model@{ suggestions, suggesting } = F.noMessages $ model
      { chatting = Nothing
      , suggesting = if DA.length suggestions <= 1 then Just 0 else suggesting
      , toggleChatModal = HideChatModal
      , editing = Nothing
      }

toggleSuggestionsFromOnline ∷ ImModel → MoreMessages
toggleSuggestionsFromOnline model = fetchMoreSuggestions model
      { suggestionsFrom = if model.suggestionsFrom == OnlineOnly then ThisWeek else OnlineOnly
      , suggestionsPage = 0
      }

setBugging ∷ MeroChatCall → ImModel → NoMessages
setBugging mc model = F.noMessages $ model
      { bugging = Just mc
      --offset index to account for non profile suggestion
      , suggesting = case  model.suggesting of
            Just s | s > 0 -> Just $ s - 1
            Just s -> Just s
            Nothing -> Nothing
      }
