module Test.Client.IM.Suggestion where

import Prelude
import Shared.IM.Types
import Shared.Types

import Client.IM.Suggestion as CIS
import Data.Array.NonEmpty (last)
import Data.Int53 as DI
import Data.Maybe (Maybe(..))
import Shared.Newtype as TSU
import Shared.PrimaryKey as SP
import Test.Unit (TestSuite)
import Test.Unit as TU
import Test.Client.Model as TCM
import Test.Unit.Assert as TUA
import Test.Unit.Main as TUM

tests :: TestSuite
tests = do
        TU.suite "im suggestion update" $ do
                TU.test "nextSuggestion sets suggesting to zero if Nothing" $ do
                        IMModel { suggesting } <- TCM.run model <<< CIS.nextSuggestion <<< TSU.updateModel model $ _ {
                                suggestions = [imUser],
                                suggesting = Nothing
                        }
                        TUA.equal (Just 0) suggesting

                TU.test "nextSuggestion bumps suggesting" $ do
                        IMModel { suggesting } <- TCM.run model <<< CIS.nextSuggestion <<< TSU.updateModel model $ _ {
                                suggestions = [imUser, imUser],
                                suggesting = Just 0
                        }
                        TUA.equal (Just 1) suggesting

                TU.test "nextSuggestion does not go over suggestions length" $ do
                        let model' = TSU.updateModel model $ _ {
                                suggestions = [imUser, imUser, imUser],
                                suggesting = Just 2
                        }
                        IMModel { suggesting } <- TCM.run model' <<< CIS.nextSuggestion $ model'
                        TUA.equal (Just 2) suggesting

                TU.test "nextSuggestion clears chatting" $ do
                        IMModel { chatting } <- TCM.run model <<< CIS.nextSuggestion <<< TSU.updateModel model $ _ {
                                chatting = Just 2,
                                suggestions = [imUser, imUser]
                        }
                        TUA.equal Nothing chatting

                TU.test "previousSuggestion descreases suggesting" $ do
                        IMModel { suggesting } <- TCM.run model <<< CIS.previousSuggestion <<< TSU.updateModel model $ _ {
                                suggestions = [imUser, imUser],
                                suggesting = Just 1
                        }
                        TUA.equal (Just 0) suggesting

                TU.test "previousSuggestion does not go bellow zero" $ do
                        let model' = TSU.updateModel model $ _ {
                                suggestions = [imUser, imUser, imUser],
                                suggesting = Just 0
                        }
                        IMModel { suggesting } <- TCM.run model' <<< CIS.previousSuggestion $ model'
                        TUA.equal (Just 0) suggesting

                TU.test "previousSuggestion clears chatting" $ do
                        IMModel { chatting } <- TCM.run model <<< CIS.previousSuggestion <<< TSU.updateModel model $ _ {
                                chatting = Just 2,
                                suggestions = [imUser, imUser]
                        }
                        TUA.equal Nothing chatting

model :: IMModel
model = IMModel {
        userContextMenuVisible: false,
        profileSettingsToggle: Hidden,
        contacts: [],
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

