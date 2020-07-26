module Client.IM.Contacts where

import Debug.Trace
import Prelude
import Shared.IM.Types
import Shared.Types

import Client.Common.Network as CCN
import Client.IM.Flame (MoreMessages, NoMessages, NextMessage)
import Client.IM.Flame as CIF
import Client.IM.Scroll as CIS
import Client.IM.WebSocket as CIM
import Client.IM.WebSocket as CIW
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Newtype as DN
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console as EC
import Flame ((:>))
import Flame as F
import Shared.Newtype as SN
import Shared.Router as SR
import Shared.Unsafe ((!@))
import Shared.Unsafe as SU
import Web.Event.Internal.Types (Event)
import Web.Socket.WebSocket (WebSocket)
import Web.UIEvent.WheelEvent (WheelEvent)
import Web.UIEvent.WheelEvent as WUW

resumeChat :: PrimaryKey -> IMModel -> MoreMessages
resumeChat searchID model@(IMModel { contacts, chatting }) = do
      let index = DA.findIndex ((searchID == _) <<< _.id <<< DN.unwrap <<< _.user <<< DN.unwrap) contacts
      if index == chatting then
            F.noMessages model
       else
            CIF.nothingNext (SN.updateModel model $ _ {
                  suggesting = Nothing,
                  chatting = index
            }) $ liftEffect CIS.scrollLastMessage

markRead :: IMModel -> MoreMessages
markRead =
      case _ of
            model@(IMModel {
                  token: Just tk,
                  user: IMUser { id: userID },
                  webSocket: Just (WS ws),
                  contacts,
                  chatting: Just index
            }) -> updateReadHistory model {
                  token: tk,
                  webSocket: ws,
                  chatting: index,
                  userID,
                  contacts
            }
            model -> CIF.nothingNext model <<< liftEffect $ EC.log "invalid markRead state"

updateReadHistory :: IMModel -> {
      token :: String,
      userID :: PrimaryKey,
      webSocket :: WebSocket,
      contacts :: Array Contact,
      chatting :: Int
} -> MoreMessages
updateReadHistory model { token, webSocket, chatting, userID, contacts } =
      let contactRead@(Contact { history }) = contacts !@ chatting
          messagesRead = DA.mapMaybe (unreadID userID) <<< _.history $ DN.unwrap contactRead
      in if DA.null messagesRead then
            CIF.nothingNext model scroll
          else
            CIF.nothingNext (updateContacts contactRead) do
                  confirmRead messagesRead
                  scroll

      where unreadID userID (HistoryMessage { recipient, id, status })
                  | status == Unread && recipient == userID = Just id
                  | otherwise = Nothing

            read userID historyEntry@(HistoryMessage { recipient, id, status })
                  | status == Unread && recipient == userID = SN.updateHistoryMessage historyEntry $ _ { status = Read }
                  | otherwise = historyEntry

            updateContacts contactRead@(Contact { history }) = SN.updateModel model $ _ {
                  contacts = SU.fromJust "updateReadHistory" $ DA.updateAt chatting (SN.updateContact contactRead $ _ { history = map (read userID) history }) contacts
            }

            confirmRead messages = liftEffect <<< CIW.sendPayload webSocket $ ReadMessages {
                  ids: messages,
                  token
            }

            scroll = liftEffect CIS.scrollLastMessage

fetchContacts :: WheelEvent -> IMModel -> MoreMessages
fetchContacts event model@(IMModel { contacts })
      | WUW.deltaY event < 1.0 =
            F.noMessages model
      | otherwise =   --needs some kind of throttling/loading
            model :> [Just <<< DisplayContacts <$> CCN.get' (Contacts { skip: DA.length contacts })]

displayContacts :: Array Contact -> IMModel -> NoMessages
displayContacts newContacts model@(IMModel { contacts })
      | DA.null newContacts =
            F.noMessages model
      | otherwise =
            F.noMessages <<< SN.updateModel model $ _ {
                  contacts = contacts <> newContacts
            }