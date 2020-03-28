module Test.Client.IM.Chat where

import Client.Common.Types
import Prelude
import Shared.Types
import Shared.PrimaryKey as SP
import Client.IM.Chat as CIC
import Data.Array ((!!), (:))
import Data.Array as DA
import Data.Int53 as DI
import Data.Maybe (Maybe(..))
import Flame (World)
import Partial.Unsafe as PU
import Effect.Now as EN
import Shared.Unsafe as SN
import Test.Unit (TestSuite)
import Shared.Newtype as SN
import Data.Either(Either(..))
import Test.Unit as TU
import Test.Unit.Assert as TUA
import Effect.Class (liftEffect)
import Unsafe.Coerce as UC
import Web.Socket.WebSocket (WebSocket)


tests :: TestSuite
tests = do
        TU.suite "im chat update" $ do
                let content = "test"

                TU.test "sendMessage bumps temporary id" $ do
                        m@(IMModel {temporaryID}) <- CIC.sendMessage webSocketHandler model content
                        TUA.equal 1 temporaryID

                        IMModel {temporaryID} <- CIC.sendMessage webSocketHandler m content
                        TUA.equal 2 temporaryID

                TU.test "sendMessage adds message to history" $ do
                        IMModel {contacts, chatting} <- CIC.sendMessage webSocketHandler model content
                        let index = SN.unsafeFromJust "test" chatting
                            IMUser user = SN.unsafeFromJust "test" (contacts !! index)

                        TUA.equal [HistoryMessage {date: Nothing, recipient: user.id, status:Unread, id: SP.fromInt 1, content, sender : user.id}] user.history

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

                let     IMUser { id: userID } = anotherIMUser
                        messageID = SP.fromInt 1
                        newMessageID = SP.fromInt 101

                TU.test "receiveMessage substitutes temporary id" $ do
                        IMModel {contacts} <- CIC.receiveMessage (SN.updateModel model $ _ {
                                contacts = [SN.updateUser anotherIMUser $ _ {
                                        history = [HistoryMessage {
                                                status: Unread,
                                                date: Nothing,
                                                id: messageID,
                                                recipient: userID,
                                                sender: userID,
                                                content
                                        }]
                                }]
                        }) $ Received {
                                previousID: messageID,
                                id : newMessageID
                        }
                        TUA.equal (getMessageID contacts) $ Just newMessageID

                date <- map MDateTime (liftEffect $ EN.nowDateTime)

                TU.test "receiveMessage adds message to history" $ do

                        IMModel {contacts} <- CIC.receiveMessage (SN.updateModel model $ _ {
                                contacts = [anotherIMUser]
                        }) $ ClientMessage {
                                date,
                                id: newMessageID,
                                content,
                                user: Right userID
                        }
                        TUA.equal (getHistory contacts) <<< Just $ HistoryMessage {
                                status: Unread,
                                id: newMessageID,
                                content,
                                sender: userID,
                                recipient: userID,
                                date: Just date
                        }

                TU.test "receiveMessage adds contact if new" $ do
                        IMModel {contacts} <- CIC.receiveMessage (SN.updateModel model $ _ {
                                contacts = []
                        }) $ ClientMessage {
                                date,
                                id: newMessageID,
                                content,
                                user: Left anotherIMUser
                        }
                        TUA.equal (DA.head contacts) <<< Just $ SN.updateUser anotherIMUser $ _ { history = [HistoryMessage { status:Unread, messageID : newMessageID, content, userID }]}

                TU.test "receiveMessage set chatting if message comes from current suggestion" $ do
                        IMModel {contacts, chatting} <- CIC.receiveMessage (SN.updateModel model $ _ {
                                contacts = [],
                                chatting = Nothing,
                                suggesting = Just 0,
                                suggestions = [anotherIMUser]
                        }) $ ClientMessage {
                                id: newMessageID,
                                content,
                                user: Left anotherIMUser
                        }
                        TUA.equal (DA.head contacts) <<< Just $ SN.updateUser anotherIMUser $ _ { history = [HistoryMessage { status:Unread, messageID : newMessageID, content, userID }]}
                        TUA.equal chatting $ Just 0

        where   getHistory contacts = do
                        IMUser {history} <- DA.head contacts
                        DA.head history
                getMessageID contacts = do
                        HistoryMessage { id } <- getHistory contacts
                        pure id

model :: IMModel
model = IMModel {
        user: imUser,
        suggestions: [imUser],
        temporaryID : 0,
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
webSocketHandler = { sendString: \_ _ -> pure unit }