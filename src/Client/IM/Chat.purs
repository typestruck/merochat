-- | This module takes care of websocket plus chat editor events.
module Client.IM.Chat(
        update,
        startChat,
        sendMessage,
        receiveMessage
) where

import Client.Common.Types
import Debug.Trace
import Prelude
import Shared.IM.Types
import Shared.Types

import Client.IM.Contacts as CICN
import Client.IM.WebSocketHandler (webSocketHandler)
import Data.Array ((:), (!!))
import Data.Array as DA
import Data.Either (Either(..))
import Data.Either as DET
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Newtype as DN
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console as EC
import Effect.Now as EN
import Flame.Application.Effectful (Environment)
import Shared.Newtype as SN
import Shared.PrimaryKey as SP
import Shared.Unsafe ((!@))
import Shared.Unsafe as SU

update :: Environment IMModel ChatMessage -> Aff IMModel
update { model, message } =
        case message of
                SendMessage content -> do
                        model' <- startChat model
                        sendMessage webSocketHandler content model'
                ReceiveMessage payload -> receiveMessage webSocketHandler model payload

startChat :: IMModel -> Aff IMModel
startChat model@(IMModel {chatting, contacts, suggesting, suggestions}) =
        pure $ case Tuple chatting suggesting of
                        Tuple Nothing (Just index) ->
                                let chatted = suggestions !@ index
                                in SN.updateModel model $ _ {
                                        chatting = Just 0,
                                        suggesting = Nothing,
                                        contacts = DA.cons chatted contacts
                                }
                        _ -> model

sendMessage :: WebSocketHandler -> String -> IMModel -> Aff IMModel
sendMessage webSocketHandler content =
        case _ of
                model@(IMModel {
                        user: IMUser { id: senderID },
                        webSocket: Just (WS webSocket),
                        token: Just token,
                        chatting: Just chatting,
                        temporaryID,
                        contacts
                }) -> do
                        date <- liftEffect $ map MDateTime EN.nowDateTime
                        let     recipient@(IMUser { id: recipientID, history }) = contacts !@ chatting
                                newTemporaryID = temporaryID + SP.fromInt 1
                                updatedChatting = SN.updateUser recipient $ _ {
                                        message = "",
                                        history = DA.snoc history $ HistoryMessage {
                                                id: newTemporaryID,
                                                status: Unread,
                                                sender: senderID,
                                                recipient: recipientID,
                                                date,
                                                content
                                        }
                                }
                        liftEffect <<< webSocketHandler.sendPayload webSocket $ ServerMessage {
                                id: newTemporaryID,
                                user: recipientID,
                                token: token,
                                content
                        }
                        pure <<< SN.updateModel model $ _ {
                                temporaryID = newTemporaryID,
                                contacts = SU.unsafeFromJust "sendMessage" $ DA.updateAt chatting updatedChatting contacts
                        }
                model -> do
                        liftEffect $ EC.log "Invalid sendMessage state"
                        pure model

receiveMessage :: WebSocketHandler -> IMModel -> WebSocketPayloadClient -> Aff IMModel
receiveMessage wsHandler model@(IMModel {
        user: IMUser { id: recipientID },
        contacts,
        suggesting,
        chatting,
        suggestions
}) =
        case _ of
                ClientMessage m@{ id, user, content, date } -> do
                        let updatedModel = case SU.unsafeFromJust "receiveMessage" $ updateHistoryMessage contacts recipientID m of
                                New contacts' ->
                                        --new messages bubble the contact to the top
                                        let added = DA.head contacts' in
                                        --edge case of recieving a message from the current suggestion
                                         if getUserID added == getUserID suggestingContact then
                                                SN.updateModel model $ _ {
                                                        contacts = contacts',
                                                        suggesting = Nothing,
                                                        chatting = Just 0
                                                }
                                          else
                                                SN.updateModel model $ _ {
                                                        contacts = contacts'
                                                }
                                Existing contacts' -> SN.updateModel model $ _ {
                                        contacts = contacts'
                                }
                        case updatedModel of
                                IMModel {
                                        token: Just tk,
                                        webSocket: Just (WS ws),
                                        chatting: Just index,
                                        contacts
                                } -> do
                                        --mark it as read if we received a message from the current chat
                                        let fields = {
                                                token: tk,
                                                webSocket: ws,
                                                chatting: index,
                                                userID: recipientID,
                                                contacts
                                        }
                                        if isChatting user fields then
                                                CICN.updateReadHistory wsHandler updatedModel fields
                                         else
                                                pure updatedModel
                                _ -> pure updatedModel
                Received { previousID, id } -> pure <<< SN.updateModel model $ _ {
                        contacts = DM.fromMaybe contacts $ updateTemporaryID contacts previousID id
                }

        where   getUserID = map (_.id <<< DN.unwrap)
                suggestingContact = do
                        index <- suggesting
                        suggestions !! index

                isChatting sender {contacts, chatting} =
                        let IMUser {id: recipientID} = contacts !@ chatting in recipientID ==  DET.either (_.id <<< DN.unwrap) identity sender

updateHistoryMessage :: Array IMUser -> PrimaryKey -> {
        id :: PrimaryKey,
        user :: Either IMUser PrimaryKey,
        date :: MDateTime,
        content :: String
} -> Maybe (ReceivedUser (Array IMUser))
updateHistoryMessage contacts recipientID { id, user, date, content } =
        case user of
                Right userID@(PrimaryKey _) -> do
                        index <- DA.findIndex (findUser userID) contacts
                        IMUser {history} <- contacts !! index

                        map Existing $ DA.modifyAt index (updateHistory { userID, content, id, date }) contacts
                Left user@(IMUser{ id: userID }) -> Just <<< New $ updateHistory { userID, content, id, date } user : contacts

        where   findUser userID (IMUser { id }) = userID == id

                updateHistory { id, userID, content, date } user@(IMUser {history}) = SN.updateUser user $ _ {
                        history = DA.snoc history $ HistoryMessage {
                                status: Unread,
                                sender: userID,
                                recipient: recipientID,
                                id,
                                content,
                                date
                        }
                }

updateTemporaryID :: Array IMUser -> PrimaryKey -> PrimaryKey -> Maybe (Array IMUser)
updateTemporaryID contacts previousID id = do
        index <- DA.findIndex (findUser previousID) contacts
        IMUser { history } <- contacts !! index
        innerIndex <- DA.findIndex (findTemporary previousID) history

        DA.modifyAt index (updateTemporary innerIndex id) contacts

        where   findTemporary previousID (HistoryMessage { id }) = id == previousID
                findUser previousID (IMUser { history }) = DA.any (findTemporary previousID) history

                updateTemporary index newID user@(IMUser { history }) = SN.updateUser user $ _ {
                        history = SU.unsafeFromJust "receiveMessage" $ DA.modifyAt index (flip SN.updateHistoryMessage (_ { id = newID })) history
                }