module Test.Client.IM.Chat where

import Client.Types
import Prelude
import Shared.Types

import Client.IM.Chat as CIC
import Data.Int53 as DI
import Data.Maybe (Maybe(..))
import Data.Newtype as DN
import Flame (World)
import Partial.Unsafe as PU
import Test.Unit (TestSuite)
import Test.Unit as TU
import Test.Unit.Assert as TUA
import Test.Unit.Main as TUM
import Unsafe.Coerce as UC
import Web.Socket.WebSocket (WebSocket)
import Data.Maybe as DM
import Data.Array ((!!))

imUser :: IMUser
imUser = IMUser {
        age: Nothing,
        name: "test",
        id: PrimaryKey $ DI.fromInt 23,
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

tests :: TestSuite
tests = do
        TU.suite "im chat update" $ do
                let content = "test"

                TU.test "sendMessage bumps temporary id" $ do
                        IMModel {temporaryID} <- CIC.sendMessage webSocketHandler model content
                        TUA.equal 1 temporaryID

                TU.test "sendMessage adds message to history" $ do
                        IMModel {suggestions, chatting} <- CIC.sendMessage webSocketHandler model content
                        let index = PU.unsafePartial $ DM.fromJust chatting
                            IMUser user = PU.unsafePartial (DM.fromJust (suggestions !! index))

                        TUA.equal [History {content}] user.history

        where   model = IMModel {
                        user: imUser,
                        suggestions: [imUser],
                        temporaryID : 0,
                        token: Just "",
                        webSocket: Just $ WS (UC.unsafeCoerce 23 :: WebSocket),
                        chatting: Just 0
                }