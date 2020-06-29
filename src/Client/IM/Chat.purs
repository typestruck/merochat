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

import Client.Common.DOM as CCD
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
import Flame.Application.Effectful (AffUpdate)
import Flame.Application.Effectful as FAE
import Shared.Newtype as SN
import Shared.PrimaryKey as SP
import Shared.Unsafe ((!@))
import Shared.Unsafe as SU

--this module needs to be tested for race conditions

update :: AffUpdate IMModel ChatMessage
update { model, message, display } =
        case message of
                SendMessage content -> do
                        model' <- startChat model
                        display (const model')
                        sendMessage webSocketHandler content model'
                ReceiveMessage payload -> do
                        isFocused <- liftEffect CCD.documentHasFocus
                        receiveMessage isFocused webSocketHandler model payload

startChat :: IMModel -> Aff IMModel
startChat model@(IMModel {chatting, contacts, suggesting, suggestions}) =
        pure $ case Tuple chatting suggesting of
                        Tuple Nothing (Just index) ->
                                let chatted = suggestions !@ index
                                in SN.updateModel model $ _ {
                                        chatting = Just 0,
                                        suggesting = Nothing,
                                        contacts = DA.cons chatted contacts,
                                        suggestions = SU.unsafeFromJust "startChat" $ DA.deleteAt index suggestions
                                }
                        _ -> model

sendMessage :: WebSocketHandler -> String -> IMModel -> Aff (IMModel -> IMModel)
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
                        FAE.diff {
                                temporaryID: newTemporaryID,
                                contacts: SU.unsafeFromJust "sendMessage" $ DA.updateAt chatting updatedChatting contacts
                        }
                model -> do
                        liftEffect $ EC.log "Invalid sendMessage state"
                        FAE.noChanges

receiveMessage :: Boolean -> WebSocketHandler -> IMModel -> WebSocketPayloadClient -> Aff (IMModel -> IMModel)
receiveMessage isFocused wsHandler model@(IMModel {
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
                                        --edge case of recieving a message from a suggestion
                                         if getUserID added == getUserID suggestingContact then
                                                SN.updateModel model $ _ {
                                                        contacts = contacts',
                                                        suggesting = Nothing,
                                                        suggestions = SU.unsafeFromJust "delete receiveMesage" do
                                                                index <- suggesting
                                                                DA.deleteAt index suggestions,
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
                                        if isFocused && isChatting user fields then
                                               CICN.updateReadHistory wsHandler updatedModel fields
                                         else
                                                pure (const updatedModel)
                                _ -> pure (const updatedModel)
                Received { previousID, id } -> FAE.diff {
                        contacts: DM.fromMaybe contacts $ updateTemporaryID contacts previousID id
                }

        where   getUserID = map (_.id <<< DN.unwrap)
                suggestingContact = do
                        index <- suggesting
                        suggestions !! index

                isChatting sender {contacts, chatting} =
                        let IMUser {id: recipientID} = contacts !@ chatting in recipientID == DET.either (_.id <<< DN.unwrap) identity sender

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
                        Contact { history } <- contacts !! index

                        map Existing $ DA.modifyAt index (updateHistory { userID, content, id, date }) contacts
                Left user@(IMUser{ id: userID }) -> Just <<< New $ updateHistory { userID, content, id, date } user : contacts

        where   findUser userID (IMUser { id }) = userID == id

                updateHistory { id, userID, content, date } user@(Contact {history}) = SN.updateContact user $ _ {
                        history = DA.snoc history $ HistoryMessage {
                                status: Unread,
                                sender: userID,
                                recipient: recipientID,
                                id,
                                content,
                                date
                        }
                }

updateTemporaryID :: Array Contact -> PrimaryKey -> PrimaryKey -> Maybe (Array Contact)
updateTemporaryID contacts previousID id = do
        index <- DA.findIndex (findUser previousID) contacts
        Contact { history } <- contacts !! index
        innerIndex <- DA.findIndex (findTemporary previousID) history

        DA.modifyAt index (updateTemporary innerIndex id) contacts

        where   findTemporary previousID (HistoryMessage { id }) = id == previousID
                findUser previousID (Contact { history }) = DA.any (findTemporary previousID) history

                updateTemporary index newID user@(Contact { history }) = SN.updateContact user $ _ {
                        history = SU.unsafeFromJust "receiveMessage" $ DA.modifyAt index (flip SN.updateHistoryMessage (_ { id = newID })) history
                }