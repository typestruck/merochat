module Client.IM.Suggestion where

import Prelude
import Shared.IM.Types
import Shared.Types

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
nextSuggestion model@(IMModel { suggestions, suggesting }) =
      let next = DM.maybe 0 (_ + 1) suggesting
      in      if next == DA.length suggestions then
                  fetchMoreSuggestions model
             else
                  F.noMessages <<< SN.updateModel model $ _ {
                        suggesting = Just next,
                        chatting = Nothing
                  }

previousSuggestion :: IMModel -> MoreMessages
previousSuggestion model@(IMModel { suggesting }) =
      let previous = DM.maybe 0 (_ - 1) suggesting
      in
            if previous < 0 then
                  fetchMoreSuggestions model
             else
                  F.noMessages <<< SN.updateModel model $ _  {
                        suggesting = Just previous,
                        chatting = Nothing
                  }

fetchMoreSuggestions :: IMModel -> NextMessage
fetchMoreSuggestions = (_ :> [Just <<< DisplayMoreSuggestions <$> CCN.get' Suggestions])

displayMoreSuggestions :: Array Suggestion -> IMModel -> NoMessages
displayMoreSuggestions suggestions =
      CIF.diff {
            suggesting: Just 0,
            chatting : Nothing,
            suggestions
      }

blockUser :: WebSocket  -> PrimaryKey -> IMModel -> NextMessage
blockUser webSocket blocked model@(IMModel { blockedUsers }) =
      updatedModel :> [do
            Ok <- CCN.post' (Block { id: blocked }) (Nothing :: NoPayload)
            liftEffect <<< CIW.sendPayload webSocket $ ToBlock { id: blocked }
            pure Nothing
      ]
      where updatedModel = removeBlockedUser blocked <<< SN.updateModel model $ _ {
                  blockedUsers = blocked : blockedUsers
            }

removeBlockedUser :: PrimaryKey -> IMModel -> IMModel
removeBlockedUser blocked model@(IMModel { contacts, suggestions }) =
      SN.updateModel model $ _ {
            contacts = DA.filter ((blocked /= _) <<< fromContact) contacts,
            suggestions = DA.filter ((blocked /= _) <<< fromUser) suggestions
      }
      where fromContact (Contact { user } ) = fromUser user
            fromUser (IMUser { id }) = id


