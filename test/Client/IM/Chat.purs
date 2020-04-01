module Test.Client.IM.Chat where

import Client.Common.Types
import Prelude
import Shared.IM.Types
import Shared.Types

import Client.IM.Chat as CIC
import Data.Array ((!!), (:))
import Data.Array as DA
import Data.Either (Either(..))
import Data.Int53 as DI
import Data.Maybe (Maybe(..))
import Data.Newtype as DN
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Now as EN
import Shared.Unsafe ((!@))
import Flame (World)
import Partial.Unsafe as PU
import Shared.Newtype as SN
import Shared.PrimaryKey as SP
import Shared.Unsafe as SN
import Test.Unit (TestSuite)
import Test.Unit as TU
import Shared.Unsafe((!@))
import Test.Unit.Assert as TUA
import Unsafe.Coerce as UC
import Web.Socket.WebSocket (WebSocket)

tests :: TestSuite
tests = do
        TU.suite "im chat update" $ do
                let content = "test"

                TU.test "sendMessage bumps temporary id" $ do
                        m@(IMModel {temporaryID}) <- CIC.sendMessage webSocketHandler content model
                        TUA.equal (SP.fromInt 1) temporaryID

                        IMModel {temporaryID} <- CIC.sendMessage webSocketHandler content m
                        TUA.equal (SP.fromInt 2) temporaryID

                TU.test "sendMessage adds message to history" $ do
                        date <- liftEffect $ map MDateTime EN.nowDateTime
                        IMModel {contacts, chatting} <- CIC.sendMessage webSocketHandler content model
                        let index = SN.unsafeFromJust "test" chatting
                            IMUser user = SN.unsafeFromJust "test" (contacts !! index)

                        TUA.equal [HistoryMessage {
                                date,
                                recipient: user.id,
                                status: Unread,
                                id: SP.fromInt 1,
                                content,
                                sender: user.id
                        }] user.history

                let IMModel { suggestions : modelSuggestions } = model

                TU.test "startChat adds new contact from suggestion" $ do
                        IMModel {contacts} <- CIC.startChat <<< SN.updateModel model $ _ {
                                suggestions = anotherIMUser : modelSuggestions,
                                chatting = Nothing,
                                suggesting = Just 0
                        }
                        TUA.equal (DA.head contacts) $ Just anotherIMUser

                TU.test "startChat resets suggesting" $ do
                        IMModel {suggesting} <- CIC.startChat <<< SN.updateModel model $ _ {
                                suggestions = anotherIMUser : modelSuggestions,
                                chatting = Nothing,
                                suggesting = Just 0
                        }
                        TUA.equal Nothing suggesting

                TU.test "startChat sets chatting to 0" $ do
                        IMModel {chatting} <- CIC.startChat <<< SN.updateModel model $ _ {
                                suggestions = anotherIMUser : modelSuggestions,
                                chatting = Nothing,
                                suggesting = Just 0
                        }
                        TUA.equal (Just 0) chatting

                let     IMUser { id: senderID } = anotherIMUser
                        IMUser { id: recipientID } = imUser
                        messageID = SP.fromInt 1
                        newMessageID = SP.fromInt 101

                TU.test "receiveMessage substitutes temporary id" $ do
                        date <- liftEffect $ map MDateTime EN.nowDateTime
                        IMModel {contacts} <- CIC.receiveMessage webSocketHandler (SN.updateModel model $ _ {
                                contacts = [SN.updateUser anotherIMUser $ _ {
                                        history = [HistoryMessage {
                                                status: Unread,
                                                date,
                                                id: messageID,
                                                recipient: recipientID,
                                                sender: senderID,
                                                content
                                        }]
                                }]
                        }) $ Received {
                                previousID: messageID,
                                id : newMessageID
                        }
                        TUA.equal (Just newMessageID) $ getMessageID contacts

                TU.test "receiveMessage adds message to history" $ do
                        date <- liftEffect $ map MDateTime EN.nowDateTime
                        IMModel {contacts} <- CIC.receiveMessage webSocketHandler (SN.updateModel model $ _ {
                                contacts = [anotherIMUser],
                                chatting = Nothing
                        }) $ ClientMessage {
                                date,
                                id: newMessageID,
                                content,
                                user: Right senderID
                        }
                        TUA.equal (Just $ HistoryMessage {
                                status: Unread,
                                id: newMessageID,
                                content,
                                sender: senderID,
                                recipient: recipientID,
                                date
                        }) $ getHistory contacts

                TU.test "receiveMessage adds contact if new" $ do
                        date <- liftEffect $ map MDateTime EN.nowDateTime
                        IMModel { contacts } <- CIC.receiveMessage webSocketHandler (SN.updateModel model $ _ {
                                contacts = [],
                                chatting = Nothing
                        }) $ ClientMessage {
                                date,
                                id: newMessageID,
                                content,
                                user: Left anotherIMUser
                        }
                        TUA.equal (_.history $ DN.unwrap (contacts !@ 0)) [
                                HistoryMessage {
                                        status: Unread,
                                        id: newMessageID,
                                        sender: senderID,
                                        recipient: recipientID,
                                        content,
                                        date
                                }
                        ]

                TU.test "receiveMessage set chatting if message comes from current suggestion" $ do
                        date <- liftEffect $ map MDateTime EN.nowDateTime
                        IMModel {contacts, chatting} <- CIC.receiveMessage webSocketHandler (SN.updateModel model $ _ {
                                contacts = [],
                                chatting = Nothing,
                                suggesting = Just 0,
                                suggestions = [anotherIMUser]
                        }) $ ClientMessage {
                                id: newMessageID,
                                user: Left anotherIMUser,
                                content,
                                date
                        }
                        TUA.equal (DA.head contacts) <<< Just $ SN.updateUser anotherIMUser $ _ { history = [HistoryMessage
                                {
                                        status: Read,
                                        id: newMessageID,
                                        sender: senderID,
                                        recipient: recipientID,
                                        date,
                                        content
                                }]
                        }
                        TUA.equal chatting $ Just 0

                TU.test "receiveMessage mark messages as read if coming from current chat" $ do
                        date <- liftEffect $ map MDateTime EN.nowDateTime
                        IMModel { contacts } <- CIC.receiveMessage webSocketHandler (SN.updateModel model $ _ {
                                contacts = [anotherIMUser],
                                chatting = Just 0,
                                suggesting = Nothing
                        }) $ ClientMessage {
                                id: newMessageID,
                                user: Left anotherIMUser,
                                content,
                                date
                        }
                        TUA.equal [Tuple newMessageID Read] <<< map (\(HistoryMessage { id, status}) -> Tuple id status) <<< _.history $ DN.unwrap (contacts !@ 0)

        where   getHistory contacts = do
                        IMUser { history } <- DA.head contacts
                        DA.head history
                getMessageID contacts = do
                        HistoryMessage { id } <- getHistory contacts
                        pure id

model :: IMModel
model = IMModel {
        user: imUser,
        suggestions: [imUser],
        temporaryID : SP.fromInt 0,
        suggesting: Just 0,
        token: Just "",
        contacts: [imUser],
        webSocket: Just $ WS (UC.unsafeCoerce 23 :: WebSocket),
        chatting: Just 0
}

imUser :: IMUser
imUser = IMUser {
        age: Nothing,
        name: "test",
        id: SP.fromInt 23,
        avatar: "",
        country: Nothing,
        languages: [],
        tags: [],
        message: "",
        history: [],
        headline: "",
        description: "",
        gender: Nothing
}

anotherIMUser :: IMUser
anotherIMUser = SN.updateUser imUser $ _ { id = SP.fromInt 90 }

world :: World _ _
world = {
        update: \a _ -> pure a,
        view: \_ -> pure unit,
        previousModel: Nothing,
        previousMessage: Nothing,
        event: Nothing
}

webSocketHandler :: WebSocketHandler
webSocketHandler = { sendPayload: \_ _ -> pure unit }