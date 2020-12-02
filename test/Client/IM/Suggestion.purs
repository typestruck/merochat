module Test.Client.IM.Suggestion where

import Prelude

import Client.IM.Suggestion as CIS
import Data.Maybe (Maybe(..))
import Data.Tuple as DT
import Test.Client.Model (model, imUser)
import Test.Unit (TestSuite)
import Test.Unit as TU
import Test.Unit.Assert as TUA

tests :: TestSuite
tests = do
      TU.suite "im suggestion update" do
            TU.test "nextSuggestion sets suggesting to zero if Nothing" do
                  let { suggesting } = DT.fst <<< CIS.nextSuggestion $ model {
                        suggestions = [imUser],
                        suggesting = Nothing
                  }
                  TUA.equal (Just 0) suggesting

            TU.test "nextSuggestion bumps suggesting" do
                  let { suggesting } = DT.fst <<< CIS.nextSuggestion $ model  {
                        suggestions = [imUser, imUser],
                        suggesting = Just 0
                  }
                  TUA.equal (Just 1) suggesting

            TU.test "nextSuggestion does not go over suggestions length" do
                  let { suggesting } = DT.fst <<< CIS.nextSuggestion $ model  {
                        suggestions = [imUser, imUser, imUser],
                        suggesting = Just 2
                  }
                  TUA.equal (Just 2) suggesting

            TU.test "blockUser removes user from suggestions" do
                  TUA.equal 1 555

            TU.test "blockUser removes user from contacts" do
                  TUA.equal 21 5558

            TU.test "blockUser resets chatting" do
                  TUA.equal 333 1

            TU.test "nextSuggestion clears chatting" do
                  let { chatting } = DT.fst <<< CIS.nextSuggestion $ model  {
                        chatting = Just 2,
                        suggestions = [imUser, imUser]
                  }
                  TUA.equal Nothing chatting

            TU.test "previousSuggestion descreases suggesting" do
                  let { suggesting } = DT.fst <<< CIS.previousSuggestion $ model  {
                        suggestions = [imUser, imUser],
                        suggesting = Just 1
                  }
                  TUA.equal (Just 0) suggesting

            TU.test "previousSuggestion does not go bellow zero" do
                  let { suggesting } = DT.fst <<< CIS.previousSuggestion $ model  {
                        suggestions = [imUser, imUser, imUser],
                        suggesting = Just 0
                  }
                  TUA.equal (Just 0) suggesting

            TU.test "displayMoreSuggestions clears chatting" do
                  let { chatting } = DT.fst <<< CIS.displayMoreSuggestions [] $ model  {
                        chatting = Just 2,
                        suggestions = [imUser, imUser]
                  }
                  TUA.equal Nothing chatting