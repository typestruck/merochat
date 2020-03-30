module Test.Client.IM.Contacts where

import Prelude
import Shared.Types

import Client.IM.Contacts as CICN
import Data.Int53 as DI
import Data.Maybe (Maybe(..))
import Flame (World)
import Shared.Newtype as SN
import Shared.PrimaryKey as SP
import Test.Unit (TestSuite)
import Test.Unit as TU
import Test.Unit.Assert as TUA
import Test.Unit.Main as TUM
import Shared.IM.Types

tests :: TestSuite
tests = do
        TU.suite "im contacts update" $ do
                TU.test "resumeChat resets suggesting" $ do
                        IMModel { suggesting } <- CICN.resumeChat (SN.updateModel model $ _ {
                                suggesting = Just 4455
                        }) $ SP.fromInt 23
                        TUA.equal Nothing suggesting

                TU.test "resumeChat sets chatting" $ do
                        m@(IMModel { chatting }) <- CICN.resumeChat (SN.updateModel model $ _ {
                                chatting = Nothing
                        }) $ SP.fromInt 32
                        TUA.equal (Just 1) chatting

                        IMModel { chatting } <- CICN.resumeChat m $ SP.fromInt 23
                        TUA.equal (Just 0) chatting

model :: IMModel
model = IMModel {
        contacts: [imUser, SN.updateUser imUser $ _ { id = SP.fromInt 32 }],
        user: imUser,
        suggestions: [],
        temporaryID : SP.fromInt 0,
        token: Nothing,
        webSocket: Nothing,
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

world :: World _ _
world = {
        update: \a _ -> pure a,
        view: \_ -> pure unit,
        previousModel: Nothing,
        previousMessage: Nothing,
        event: Nothing
}
