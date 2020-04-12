module Client.IM.Contacts where

import Client.Common.Types
import Debug.Trace
import Prelude
import Shared.IM.Types
import Shared.Types

import Client.IM.WebSocketHandler (webSocketHandler)
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Newtype as DN
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console as EC
import Flame.Application.Effectful (Environment)
import Shared.Newtype as SN
import Shared.Unsafe ((!@))
import Shared.Unsafe as SU
import Web.Socket.WebSocket (WebSocket)

update :: Environment IMModel ContactMessage -> Aff IMModel
update { model, message } =
        case message of
                ResumeChat id -> do
                        model' <- resumeChat id model
                        markRead webSocketHandler  model'

markRead :: WebSocketHandler -> IMModel -> Aff IMModel
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
                        pure model

updateReadHistory :: WebSocketHandler -> IMModel -> {
        token :: String,
        userID :: PrimaryKey,
        webSocket :: WebSocket,
        contacts :: Array IMUser,
        chatting :: Int
} -> Aff IMModel
updateReadHistory wsHandler model { token, webSocket, chatting, userID, contacts } = do
        let readContact@(IMUser { history }) = contacts !@ chatting
        liftEffect <<< wsHandler.sendPayload webSocket $ ReadMessages {
                ids: DA.mapMaybe (unreadID userID) <<< _.history $ DN.unwrap readContact,
                token
        }
        pure <<< SN.updateModel model $ _ {
                contacts = SU.unsafeFromJust "markRead" $ DA.updateAt chatting (SN.updateUser readContact $ _ { history = map (read userID) history }) contacts
        }
        where   unreadID userID  (HistoryMessage { recipient, id, status })
                        | status == Unread && recipient == userID = Just id
                        | otherwise = Nothing

                read userID historyEntry@(HistoryMessage { recipient, id, status })
                        | status == Unread && recipient == userID = SN.updateHistoryMessage historyEntry $ _ { status = Read }
                        | otherwise = historyEntry

resumeChat :: PrimaryKey -> IMModel -> Aff IMModel
resumeChat searchID model@(IMModel { contacts }) =
        pure <<< SN.updateModel model $ _ {
                suggesting = Nothing,
                chatting = DA.findIndex (\(IMUser {id}) -> searchID == id) contacts
        }


