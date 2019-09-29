module Test.Client.IM.Suggestion where

import Prelude
import Shared.Types

import Client.IM.Suggestion as CIS
import Data.Int53 as DI
import Data.Maybe (Maybe(..))
import Flame (World)
import Test.Unit (TestSuite)
import Test.Unit as TU
import Test.Unit.Assert as TUA
import Test.Unit.Main as TUM

imUser :: IMUser
imUser = IMUser {
        age: Nothing,
        name: "test",
        id: PrimaryKey $ DI.fromInt 23,
        avatar: "",
        country: Nothing,
        languages: [],
        tags: [],
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

tests :: TestSuite
tests = do
        TU.suite "im suggestion update" $ do
                TU.test "next suggestion" $ do
                        let model = IMModel {
                                user: imUser,
                                suggestions: [],
                                chatting: Nothing
                        }
                        updatedModel <- CIS.update world model NextSuggestion
                        TUA.equal model updatedModel