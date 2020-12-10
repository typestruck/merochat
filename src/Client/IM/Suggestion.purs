module Client.IM.Suggestion where

import Prelude
import Shared.Types

import Client.Common.Network (request)
import Client.Common.Network as CCN
import Client.IM.Flame (NextMessage, NoMessages, MoreMessages)
import Client.IM.WebSocket as CIW
import Data.Array ((:))
import Data.Array as DA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Debug.Trace (spy)
import Effect.Class (liftEffect)
import Flame ((:>))
import Flame as F
import Shared.Options.Page (suggestionsPerPage)
import Web.Socket.WebSocket (WebSocket)

nextSuggestion :: IMModel -> MoreMessages
nextSuggestion model@{ suggestions, suggesting } =
      let next = DM.maybe 0 (_ + 1) suggesting
      in    if next == DA.length suggestions then
                  fetchMoreSuggestions model
             else
                  F.noMessages $ model {
                        freeToFetchSuggestions = true,
                        suggesting = Just next,
                        chatting = Nothing
                  }

previousSuggestion :: IMModel -> MoreMessages
previousSuggestion model@{ suggesting } =
      let previous = DM.maybe 0 (_ - 1) suggesting
      in
            if previous < 0 then
                  fetchMoreSuggestions model
             else
                  F.noMessages $ model  {
                        freeToFetchSuggestions = true,
                        suggesting = Just previous,
                        chatting = Nothing
                  }

fetchMoreSuggestions :: IMModel -> NextMessage
fetchMoreSuggestions model@{ suggestionsPage } = model {
      freeToFetchSuggestions = false,
      failedRequests = []
} :> [CCN.retryableResponse NextSuggestion DisplayMoreSuggestions $ request.im.suggestions { query: { skip: suggestionsPerPage * suggestionsPage }}]

displayMoreSuggestions :: Array Suggestion -> IMModel -> MoreMessages
displayMoreSuggestions suggestions model@{ suggestionsPage }
      --if we looped through all the suggestions, retry
      | DA.null suggestions && suggestionsPage > 0 = fetchMoreSuggestions $ model { suggestionsPage = 0 }
      | otherwise =
            F.noMessages $ model {
                  suggesting = Just 1,
                  chatting = Nothing,
                  freeToFetchSuggestions = true,
                  suggestions = suggestions,
                  suggestionsPage = if DA.null suggestions then 0 else suggestionsPage + 1
            }

blockUser :: WebSocket  -> PrimaryKey -> IMModel -> NextMessage
blockUser webSocket blocked model@{ blockedUsers } =
      updatedModel :> [do
            result <- CCN.defaultResponse $ request.im.block { query: { id: blocked } }
            case result of
                  Left _ -> pure <<< Just $ RequestFailed { request: BlockUser blocked, errorMessage : "" }
                  _ -> do
                        liftEffect <<< CIW.sendPayload webSocket $ ToBlock { id: blocked }
                        pure Nothing
      ]
      where updatedModel = removeBlockedUser blocked $ model {
                  blockedUsers = blocked : blockedUsers,
                  chatting = Nothing,
                  failedRequests = [],
                  toggleContextMenu = HideContextMenu
            }

removeBlockedUser :: PrimaryKey -> IMModel -> IMModel
removeBlockedUser blocked model@{ contacts, suggestions } =
      model {
            contacts = DA.filter ((blocked /= _) <<< fromContact) contacts,
            suggestions = DA.filter ((blocked /= _) <<< fromUser) suggestions
      }
      where fromContact { user } = fromUser user
            fromUser { id } = id

toggleContactProfile :: IMModel -> NoMessages
toggleContactProfile model@{ fullContactProfileVisible } = F.noMessages $ model {
      fullContactProfileVisible = not fullContactProfileVisible
}

resumeSuggesting :: IMModel -> NoMessages
resumeSuggesting model = F.noMessages $ model {
      chatting = Nothing
}