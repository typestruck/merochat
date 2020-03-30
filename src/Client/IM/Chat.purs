-- | This module takes care of websocket plus chat editor events.
module Client.IM.Chat(
        webSocketHandler,
        update,
        startChat,
        sendMessage,
        receiveMessage
) where

import Client.Common.Types
import Debug.Trace
import Prelude
import Shared.Types
import Shared.IM.Types

import Data.Argonaut.Core as DAC
import Data.Argonaut.Encode.Generic.Rep as DAEGR
import Data.Array ((:), (!!))
import Data.Array as DA
import Data.Array.NonEmpty as DAN
import Data.Either (Either(..))
import Data.Int53 as DI
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff as EA
import Effect.Class (liftEffect)
import Effect.Console as EC
import Effect.Now as EN
import Data.Newtype as DN
import Flame (World)
import Partial.Unsafe as PU
import Shared.JSON as SJ
import Shared.Newtype as SN
import Shared.PrimaryKey as SP
import Shared.Unsafe ((!@))
import Shared.Unsafe as SU
import Web.Socket.WebSocket as WSW

webSocketHandler :: WebSocketHandler
webSocketHandler = { sendString: WSW.sendString }

update :: World IMModel IMMessage -> IMModel -> ChatMessage -> Aff IMModel
update _ model =
        case _ of
                SendMessage content -> do
                        model' <- startChat model
                        sendMessage webSocketHandler content model'
                ReceiveMessage payload -> receiveMessage model payload

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
                        user: IMUser {id: senderID},
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
                        liftEffect <<< webSocketHandler.sendString webSocket <<< SJ.toJSON $ ServerMessage {
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

receiveMessage :: IMModel -> WebSocketPayloadClient -> Aff IMModel
receiveMessage model@(IMModel {
        user: IMUser { id: recipientID },
        contacts,
        suggesting,
        suggestions
}) =
        case _ of
                ClientMessage m@{ id, user, content, date } ->
                        case SU.unsafeFromJust "receiveMessage" $ updateHistoryMessage contacts recipientID m of
                                New contacts' -> do
                                        --new messages bubble the contact to the top
                                        let added = DA.head contacts'
                                        --edge case of recieving a message from the current suggestion
                                        pure $ if getUserID added == getUserID suggestingContact then
                                                        SN.updateModel model $ _ {
                                                                contacts = contacts',
                                                                suggesting = Nothing,
                                                                chatting = Just 0
                                                        }
                                                else
                                                        SN.updateModel model $ _ {
                                                                contacts = contacts'
                                                        }
                                Existing contacts' -> pure <<< SN.updateModel model $ _ {
                                        contacts = contacts'
                                }
                Received { previousID, id } -> pure <<< SN.updateModel model $ _ {
                        contacts = DM.fromMaybe contacts $ updateTemporaryID contacts previousID id
                }

        where   getUserID = map (_.id <<< DN.unwrap)
                suggestingContact = do
                        index <- suggesting
                        suggestions !! index

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