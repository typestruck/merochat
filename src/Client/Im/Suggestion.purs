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
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Tuple (Tuple)
import Data.Tuple as DT
import Debug (spy)
import Effect.Class (liftEffect)
import Data.Tuple.Nested ((/\))
import Flame as F
import Shared.Options.Page (suggestionsPerPage)
import Web.Socket.WebSocket (WebSocket)

nextSuggestion ∷ ImModel → MoreMessages
nextSuggestion model@{ suggestions, suggesting } =
      let
            next = DM.maybe 0 (_ + 1) suggesting
      in
            if next == DA.length suggestions then
                  fetchMoreSuggestions model
            else
                  F.noMessages $ model
                        { freeToFetchSuggestions = true
                        , suggesting = Just next
                        , chatting = Nothing
                        }

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
fetchMoreSuggestions model@{ contacts, suggestionsPage } =
      model
            { freeToFetchSuggestions = false
            , failedRequests = []
            } /\
            [ CCN.retryableResponse NextSuggestion DisplayMoreSuggestions $ request.im.suggestions
                    { query:
                            { skip: suggestionsPerPage * suggestionsPage
                            }
                    }
            ]

displayMoreSuggestions ∷ Array Suggestion → ImModel → MoreMessages
displayMoreSuggestions suggestions model@{ suggestionsPage } =
      --if we looped through all the suggestions, retry
      if suggestionsSize == 0 && suggestionsPage > 0 then
            fetchMoreSuggestions $ model
                  { suggestionsPage = 0
                  , suggesting = suggesting
                  }
      else
            F.noMessages $ model
                  { suggesting = suggesting
                  , chatting = Nothing
                  , freeToFetchSuggestions = true
                  , suggestions = suggestions
                  , suggestionsPage = if suggestionsSize == 0 then 0 else suggestionsPage + 1
                  }
      where
      suggestionsSize = DA.length suggestions
      suggesting = Just $ if suggestionsSize <= 1 then 0 else 1

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
      }