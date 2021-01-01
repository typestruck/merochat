module Test.Client.IM.Suggestion where

import Prelude

import Client.IM.Suggestion as CIS
import Data.Maybe (Maybe(..))
import Data.Tuple as DT
import Test.Client.Model (contact, imUser, model, webSocket)
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

            TU.test "displayMoreSuggestions sets suggesting to 0 if there is 1 or fewer new suggestions" do
                  let { suggesting } = DT.fst <<< CIS.displayMoreSuggestions [imUser] $ model  {
                        suggesting = Nothing,
                        suggestions = []
                  }
                  TUA.equal (Just 0) suggesting

            TU.test "blockUser removes user from suggestions" do
                  let { suggestions } = DT.fst <<< CIS.blockUser webSocket imUser.id $ model {
                        suggestions = [imUser]
                  }
                  TUA.equal [] suggestions

            TU.test "blockUser removes user from contacts" do
                  let { contacts } = DT.fst <<< CIS.blockUser webSocket contact.user.id $ model {
                        contacts = [contact]
                  }
                  TUA.equal [] contacts

            TU.test "blockUser resets chatting" do
                  let { chatting } = DT.fst <<< CIS.blockUser webSocket contact.user.id $ model {
                        contacts = [contact]
                  }
                  TUA.equal Nothing chatting

            TU.test "resumeSuggesting sets suggestions to 0 if there is 1 or fewer suggestions" do
                  let { suggesting } = DT.fst <<< CIS.resumeSuggesting $ model  {
                        suggesting = Nothing,
                        suggestions = []
                  }
                  TUA.equal (Just 0) suggesting
