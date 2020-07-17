module Client.IM.Contacts where

import Client.Common.Types
import Debug.Trace
import Prelude
import Shared.IM.Types
import Shared.Types

import Client.Common.Network as CCN
import Client.IM.Flame (NoMessages, MoreMessages)
import Client.IM.Flame as CIF
import Client.IM.WebSocket as CIM
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Newtype as DN
import Effect.Aff (Aff)
import Client.IM.WebSocket as CIW
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

update :: IMModel -> ContactMessage -> MoreMessages
update model  =
      case _ of
            ResumeChat id -> resumeChat id model
            MarkAsRead -> markRead model
            --when the window is focused updated the read status of current chat
            UpdateReadCount -> markRead model
            FetchContacts event -> fetchContacts (SU.fromJust "contacts.update" $ WUW.fromEvent event) model
            DisplayContacts (JSONResponse contacts) -> displayContacts contacts model

resumeChat :: PrimaryKey -> IMModel -> NoMessages
resumeChat searchID model@(IMModel { contacts }) =
      F.noMessages <<< SN.updateModel model $ _ {
            suggesting = Nothing,
            chatting = DA.findIndex ((searchID == _) <<< _.id <<< DN.unwrap <<< _.user <<< DN.unwrap) contacts
      }

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
            F.noMessages model
          else
            CIF.nothingNext (updateContacts contactRead) (confirmRead messagesRead)
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

fetchContacts :: WheelEvent -> IMModel -> MoreMessages
fetchContacts event model@(IMModel { contacts })
      | WUW.deltaY event < 1.0 =
            F.noMessages model
      | otherwise =   --needs some kind of throttling/loading
            model :> [Just <<< CNM <<< DisplayContacts <$> CCN.get' (Contacts { skip: DA.length contacts })]

displayContacts :: Array Contact -> IMModel -> NoMessages
displayContacts newContacts model@(IMModel { contacts })
      | DA.null newContacts =
            F.noMessages model
      | otherwise =
            F.noMessages <<< SN.updateModel model $ _ {
                  contacts = contacts <> newContacts
            }