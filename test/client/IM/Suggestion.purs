module Test.Client.IM.Suggestion where

import Prelude

import Shared.Types

import Client.IM.Suggestion as CIS
import Data.Array.NonEmpty (last)
import Data.Int53 as DI
import Data.Maybe (Maybe(..))
import Data.Tuple as DT
import Shared.Newtype as TSU
import Shared.PrimaryKey as SP
import Test.Client.Model (model, imUser)
import Test.Client.Model as TCM
import Test.Unit (TestSuite)
import Test.Unit as TU
import Test.Unit.Assert as TUA
import Test.Unit.Main as TUM

tests :: TestSuite
tests = do
        TU.suite "im suggestion update" do
                TU.test "nextSuggestion sets suggesting to zero if Nothing" do
                        let IMModel { suggesting } = DT.fst <<< CIS.nextSuggestion <<< TSU.updateModel model $ _ {
                                suggestions = [imUser],
                                suggesting = Nothing
                        }
                        TUA.equal (Just 0) suggesting

                TU.test "nextSuggestion bumps suggesting" do
                        let IMModel { suggesting } = DT.fst <<< CIS.nextSuggestion <<< TSU.updateModel model $ _ {
                                suggestions = [imUser, imUser],
                                suggesting = Just 0
                        }
                        TUA.equal (Just 1) suggesting

                TU.test "nextSuggestion does not go over suggestions length" do
                        let IMModel { suggesting } = DT.fst <<< CIS.nextSuggestion <<< TSU.updateModel model $ _ {
                                suggestions = [imUser, imUser, imUser],
                                suggesting = Just 2
                        }
                        TUA.equal (Just 2) suggesting

                TU.test "nextSuggestion clears chatting" do
                        let IMModel { chatting } = DT.fst <<< CIS.nextSuggestion <<< TSU.updateModel model $ _ {
                                chatting = Just 2,
                                suggestions = [imUser, imUser]
                        }
                        TUA.equal Nothing chatting

                TU.test "previousSuggestion descreases suggesting" do
                        let IMModel { suggesting } = DT.fst <<< CIS.previousSuggestion <<< TSU.updateModel model $ _ {
                                suggestions = [imUser, imUser],
                                suggesting = Just 1
                        }
                        TUA.equal (Just 0) suggesting

                TU.test "previousSuggestion does not go bellow zero" do
                        let IMModel { suggesting } = DT.fst <<< CIS.previousSuggestion <<< TSU.updateModel model $ _ {
                                suggestions = [imUser, imUser, imUser],
                                suggesting = Just 0
                        }
                        TUA.equal (Just 0) suggesting

                TU.test "displayMoreSuggestions clears chatting" do
                        let IMModel { chatting } = DT.fst <<< CIS.displayMoreSuggestions [] <<< TSU.updateModel model $ _ {
                                chatting = Just 2,
                                suggestions = [imUser, imUser]
                        }
                        TUA.equal Nothing chatting