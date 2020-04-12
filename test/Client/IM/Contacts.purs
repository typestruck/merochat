module Test.Client.IM.Contacts where

import Client.Common.Types
import Prelude
import Shared.IM.Types
import Shared.Types

import Client.IM.Contacts as CICN
import Data.Int53 as DI
import Data.Maybe (Maybe(..))
import Data.Newtype as DN
import Data.Tuple (Tuple(..))
import Effect.Now as EN
import Effect.Unsafe as EU

import Shared.Newtype as SN
import Shared.PrimaryKey as SP
import Shared.Unsafe ((!@))
import Test.Unit (TestSuite)
import Test.Unit as TU
import Test.Unit.Assert as TUA
import Test.Unit.Main as TUM
import Unsafe.Coerce (unsafeCoerce)
import Web.Socket.WebSocket (WebSocket)

tests :: TestSuite
tests = do
        TU.suite "im contacts update" $ do
                TU.test "resumeChat resets suggesting" $ do
                        IMModel { suggesting } <- CICN.resumeChat (SP.fromInt 23) <<< SN.updateModel model $ _ {
                                suggesting = Just 4455
                        }
                        TUA.equal Nothing suggesting

                TU.test "resumeChat sets chatting" $ do
                        m@(IMModel { chatting }) <- CICN.resumeChat (SP.fromInt 32) <<< SN.updateModel model $ _ {
                                chatting = Nothing
                        }
                        TUA.equal (Just 1) chatting

                        IMModel { chatting } <- CICN.resumeChat (SP.fromInt 23) m
                        TUA.equal (Just 0) chatting
                TU.test "markRead sets recieved messages as read" $ do
                        IMModel { contacts } <- CICN.markRead webSocketHandler $ SN.updateModel model $ _ {
                                chatting = Just 1
                        }
                        TUA.equal [Tuple (SP.fromInt 1) Read, Tuple (SP.fromInt 2) Unread, Tuple (SP.fromInt 3) Read] <<< map (\(HistoryMessage { id, status}) -> Tuple id status) <<< _.history $ DN.unwrap (contacts !@ 1)

model :: IMModel
model = IMModel {
        contacts: [imUser, anotherIMUser],
        user: imUser,
        suggestions: [],
        temporaryID : SP.fromInt 0,
        token: Just "oi",
        webSocket: Just $ WS (unsafeCoerce 23 :: WebSocket),
        suggesting: Just 0,
        chatting: Nothing
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
anotherIMUser = SN.updateUser imUser $ _ {
        id = SP.fromInt 32,
        history = [
                HistoryMessage {
                       id: SP.fromInt 1,
                       status: Unread,
                       sender: SP.fromInt 32,
                       recipient: _.id $ DN.unwrap imUser,
                       content: "1",
                       date: EU.unsafePerformEffect $ map MDateTime EN.nowDateTime
                },
                HistoryMessage {
                       id: SP.fromInt 2,
                       status: Unread,
                       sender: _.id $ DN.unwrap imUser,
                       recipient: SP.fromInt 32,
                       content: "2",
                       date: EU.unsafePerformEffect $ map MDateTime EN.nowDateTime
                },
                HistoryMessage {
                       id: SP.fromInt 3,
                       status: Unread,
                       sender: SP.fromInt 32,
                       recipient: _.id $ DN.unwrap imUser,
                       content: "3",
                       date: EU.unsafePerformEffect $ map MDateTime EN.nowDateTime
                }
        ]
}

world :: Environment _ _
world = {
        update: \a _ -> pure a,
        view: \_ -> pure unit,
        previousModel: Nothing,
        previousMessage: Nothing,
        event: Nothing
}

webSocketHandler :: WebSocketHandler
webSocketHandler = { sendPayload: \_ _ -> pure unit }