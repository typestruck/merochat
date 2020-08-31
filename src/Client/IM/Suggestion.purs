module Client.IM.Suggestion where

import Prelude
import Shared.Types

import Client.Common.Network (request)
import Client.Common.Network as CCN
import Client.IM.Flame (NoMessages, MoreMessages, NextMessage)
import Client.IM.Flame as CIF
import Client.IM.Flame as CIF
import Client.IM.WebSocket as CIW
import Control.Alt ((<|>))
import Data.Array ((!!), (:))
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Tuple (Tuple(..))
import Debug.Trace (spy)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Flame ((:>))
import Flame as F
import Shared.Newtype as SN
import Shared.Unsafe as SU
import Web.Socket.WebSocket (WebSocket)

nextSuggestion :: IMModel -> MoreMessages
nextSuggestion model@{ suggestions, suggesting } =
      let next = DM.maybe 0 (_ + 1) suggesting
      in      if next == DA.length suggestions then
                  fetchMoreSuggestions model
             else
                  F.noMessages $ model {
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
                        suggesting = Just previous,
                        chatting = Nothing
                  }

fetchMoreSuggestions :: IMModel -> NextMessage
fetchMoreSuggestions = (_ :> [Just <<< DisplayMoreSuggestions <$> CCN.response (request.im.suggestions {}) ])

displayMoreSuggestions :: Array Suggestion -> IMModel -> NoMessages
displayMoreSuggestions suggestions =
      CIF.diff {
            suggesting: Just 0,
            chatting : Nothing,
            suggestions
      }

blockUser :: WebSocket  -> PrimaryKey -> IMModel -> NextMessage
blockUser webSocket blocked model@{ blockedUsers } =
      updatedModel :> [do
            void <<< CCN.response $ request.im.block { query: { id: blocked } }
            liftEffect <<< CIW.sendPayload webSocket $ ToBlock { id: blocked }
            pure Nothing
      ]
      where updatedModel = removeBlockedUser blocked $ model {
                  blockedUsers = blocked : blockedUsers
            }

removeBlockedUser :: PrimaryKey -> IMModel -> IMModel
removeBlockedUser blocked model@{ contacts, suggestions } =
      model {
            contacts = DA.filter ((blocked /= _) <<< fromContact) contacts,
            suggestions = DA.filter ((blocked /= _) <<< fromUser) suggestions
      }
      where fromContact { user } = fromUser user
            fromUser { id } = id


