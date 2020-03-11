module Client.IM.Chat where

import Client.Common
import Client.Common.Types
import Prelude
import Shared.Types

import Data.Argonaut.Core as DAC
import Data.Argonaut.Encode.Generic.Rep as DAEGR
import Data.Array ((:), (!!))
import Data.Array as DA
import Data.Array.NonEmpty as DAN
import Data.Int53 as DI
import Data.Maybe (Maybe(..))
import Debug.Trace
import Data.Maybe as DM
import Effect.Aff (Aff)
import Effect.Aff as EA
import Effect.Class (liftEffect)
import Shared.Unsafe((!@))
import Data.Either (Either(..))
import Shared.Unsafe as SU
import Shared.PrimaryKey as SP
import Data.Tuple(Tuple(..))
import Effect.Console as EC
import Flame (World)
import Partial.Unsafe as PU
import Shared.JSON as SJ
import Web.Socket.WebSocket as WSW

webSocketHandler :: WebSocketHandler
webSocketHandler = { sendString: WSW.sendString }

update :: World IMModel IMMessage -> IMModel -> ChatMessage -> Aff IMModel
update _ model =
        case _ of
                SendMessage content -> do
                        model' <- startChat model
                        sendMessage webSocketHandler model' content
                ReceiveMessage payload -> receiveMessage model payload

startChat :: IMModel -> Aff IMModel
startChat immodel@(IMModel model@{chatting, contacts, suggesting, suggestions}) =
        pure $ case Tuple chatting suggesting of
                        Tuple Nothing (Just index) ->
                                let chatted = suggestions !@ index
                                in IMModel $ model {
                                        chatting = Just 0,
                                        suggesting = Nothing,
                                        contacts = DA.cons chatted contacts
                                }
                        _ -> immodel

sendMessage :: WebSocketHandler -> IMModel -> String -> Aff IMModel
sendMessage webSocketHandler (IMModel model@{user: IMUser {id: senderID}, webSocket: Just (WS webSocket), token: Just token, temporaryID, chatting: Just chatting, contacts}) content = do
        let     (IMUser user) = SU.unsafeFromJust $ contacts !! chatting
                newTemporaryID = temporaryID + 1
                updatedChatting = IMUser $ user {
                        message = "",
                        history = DA.snoc user.history $ History {
                                messageID: SP.fromInt newTemporaryID,
                                userID: senderID,
                                content
                        }
                }
        liftEffect <<< webSocketHandler.sendString webSocket <<< SJ.toJSON $ ServerMessage {
                id: SP.fromInt newTemporaryID,
                user: user.id,
                token: token,
                content
        }
        pure <<< IMModel $ model {
                temporaryID = newTemporaryID,
                contacts = SU.unsafeFromJust $ DA.updateAt chatting updatedChatting contacts
        }
sendMessage _ model _ = do
        liftEffect $ EC.log "Invalid sendMessage state"
        pure model

receiveMessage :: IMModel -> WebSocketPayloadClient -> Aff IMModel
receiveMessage m@(IMModel model@{contacts, suggesting, suggestions}) payload = do
        case payload of
                ClientMessage m@{ id, user, content } -> do
                        let contacts = SU.unsafeFromJust $ findUpdateContent m
                        case contacts of 
                                New contacts' -> do 
                                        --new messages bubble the contact to the top
                                        let added = DA.head contacts'
                                        --edge case of recieving a message from the current suggestion
                                        pure $ if getUserID added == getUserID suggestingContact then 
                                                        IMModel $ model {
                                                                contacts = contacts',
                                                                suggesting = Nothing,
                                                                chatting = Just 0
                                                        }
                                                else  
                                                        IMModel $ model {
                                                                contacts = contacts'
                                                        }
                                Existing contacts' -> do 
                                        pure <<< IMModel $ model {
                                                contacts = contacts'
                                        }
                Received { previousID, id } -> pure <<< IMModel $ model {
                        contacts = DM.fromMaybe contacts $ findUpdateTemporaryID previousID id
                }
        where   getUserID = map (\(IMUser {id}) -> id)
                suggestingContact = do
                        index <- suggesting
                        suggestions !! index

                findTemporary previousID (History { messageID }) = messageID == previousID
                findUserByMessage previousID (IMUser {history}) = DA.any (findTemporary previousID) history
                updateTemporary index newID (IMUser user@{history}) = IMUser $ user {
                        history = SU.unsafeFromJust $ DA.modifyAt index (\(History history) -> History $ history {
                                messageID = newID
                        }) history
                }
                findUpdateTemporaryID previousID id = do                                
                        index <- DA.findIndex (findUserByMessage previousID) contacts
                        IMUser {history} <- contacts !! index
                        innerIndex <- DA.findIndex (findTemporary previousID) history                        

                        DA.modifyAt index (updateTemporary innerIndex id) contacts

                findUserByID userID (IMUser {id}) = userID == id
                updateContent { messageID, userID, content } (IMUser user@{history}) = IMUser $ user {
                        history = DA.snoc history $ History { messageID, userID, content }
                }
                findUpdateContent { id, user, content } = do                                
                        case user of 
                                Right userID@(PrimaryKey _) -> do
                                        index <- DA.findIndex (findUserByID userID) contacts
                                        IMUser {history} <- contacts !! index

                                        map Existing $ DA.modifyAt index (updateContent { userID, content, messageID : id }) contacts
                                Left user@(IMUser{ id: userID }) -> Just <<< New $ updateContent ({ userID, content, messageID : id }) user : contacts
                                