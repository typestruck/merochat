module Client.IM.Contacts where

import Client.Common.Types
import Debug.Trace
import Prelude
import Shared.IM.Types
import Shared.Types

import Client.Common.Network as CCN
import Client.IM.WebSocketHandler (webSocketHandler)
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Newtype as DN
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console as EC
import Flame.Application.Effectful (AffUpdate)
import Flame.Application.Effectful as FAE
import Shared.Newtype as SN
import Shared.Router as SR
import Shared.Unsafe ((!@))
import Shared.Unsafe as SU
import Web.Event.Internal.Types (Event)
import Web.Socket.WebSocket (WebSocket)
import Web.UIEvent.WheelEvent (WheelEvent)
import Web.UIEvent.WheelEvent as WUW

update :: AffUpdate IMModel ContactMessage
update { model, message, display } =
        case message of
                ResumeChat id -> do
                        model' <- resumeChat id model
                        display (const model')
                        markRead webSocketHandler model'
                --when the window is focused updated the read status of current chat
                UpdateReadCount -> markRead webSocketHandler model
                MoreContacts event -> fetchContacts (SU.unsafeFromJust "contacts.update" $ WUW.fromEvent event) model

--will need throttling or a loading screen
fetchContacts :: WheelEvent -> IMModel -> Aff (IMModel -> IMModel)
fetchContacts event (IMModel { contactsPage, contacts }) = do
        if WUW.deltaY event < 1.0 then
                FAE.noChanges
         else do
                let nextPage = contactsPage + 1
                JSONResponse newContatcs <- CCN.get' (SR.fromRoute $ Contacts { page: nextPage })
                FAE.diff { contactsPage : nextPage, contacts: contacts <> newContatcs }

markRead :: WebSocketHandler -> IMModel -> Aff (IMModel -> IMModel)
markRead wsHandler =
        case _ of
                model@(IMModel {
                        token: Just tk,
                        user: IMUser { id: userID },
                        webSocket: Just (WS ws),
                        contacts,
                        chatting: Just index
                }) -> updateReadHistory wsHandler model {
                        token: tk,
                        webSocket: ws,
                        chatting: index,
                        userID,
                        contacts
                }
                model -> do
                        liftEffect $ EC.log "invalid markRead state"
                        FAE.noChanges

updateReadHistory :: WebSocketHandler -> IMModel -> {
        token :: String,
        userID :: PrimaryKey,
        webSocket :: WebSocket,
        contacts :: Array IMUser,
        chatting :: Int
} -> Aff (IMModel -> IMModel)
updateReadHistory wsHandler model { token, webSocket, chatting, userID, contacts } = do
        let     contactRead@(IMUser { history }) = contacts !@ chatting
                messagesRead = DA.mapMaybe (unreadID userID) <<< _.history $ DN.unwrap contactRead

        if DA.null messagesRead then
                FAE.noChanges
         else do
                liftEffect <<< wsHandler.sendPayload webSocket $ ReadMessages {
                        ids: messagesRead,
                        token
                }
                FAE.diff {
                        contacts: SU.unsafeFromJust "updateReadHistory" $ DA.updateAt chatting (SN.updateUser contactRead $ _ { history = map (read userID) history }) contacts
                }
        where   unreadID userID (HistoryMessage { recipient, id, status })
                        | status == Unread && recipient == userID = Just id
                        | otherwise = Nothing

                read userID historyEntry@(HistoryMessage { recipient, id, status })
                        | status == Unread && recipient == userID = SN.updateHistoryMessage historyEntry $ _ { status = Read }
                        | otherwise = historyEntry

resumeChat :: PrimaryKey -> IMModel -> Aff IMModel
resumeChat searchID model@(IMModel { contacts }) =
        pure <<< SN.updateModel model $ _ {
                suggesting = Nothing,
                chatting = DA.findIndex (\(IMUser { id }) -> searchID == id) contacts
        }


