module Test.Client.Im.Suggestion where

import Prelude

import Client.Im.Main as CIM
import Client.Im.Suggestion as CIS
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple as DT
import Test.Client.Model (contact, imUser, model, webSocket)
import Test.Unit (TestSuite)
import Test.Unit as TU
import Test.Unit.Assert as TUA

tests ∷ TestSuite
tests = do
      TU.suite "im suggestion update" do
            TU.test "nextSuggestion bumps suggesting" do
                  let
                        { suggesting } = DT.fst <<< CIS.nextSuggestion $ model
                              { suggestions = [ imUser, imUser ]
                              , suggesting = 0
                              }
                  TUA.equal 1 suggesting

            TU.test "nextSuggestion does not go over suggestions length" do
                  let
                        { suggesting } = DT.fst <<< CIS.nextSuggestion $ model
                              { suggestions = [ imUser, imUser, imUser ]
                              , suggesting = 2
                              }
                  TUA.equal 2 suggesting

            TU.test "nextSuggestion clears chatting" do
                  let
                        { chatting } = DT.fst <<< CIS.nextSuggestion $ model
                              { chatting = Just 2
                              , suggestions = [ imUser, imUser ]
                              }
                  TUA.equal Nothing chatting

            TU.test "previousSuggestion descreases suggesting" do
                  let
                        { suggesting } = DT.fst <<< CIS.previousSuggestion $ model
                              { suggestions = [ imUser, imUser ]
                              , suggesting = 1
                              }
                  TUA.equal 0 suggesting

            TU.test "previousSuggestion does not go bellow zero" do
                  let
                        { suggesting } = DT.fst <<< CIS.previousSuggestion $ model
                              { suggestions = [ imUser, imUser, imUser ]
                              , suggesting = 0
                              }
                  TUA.equal 0 suggesting

            TU.test "displayMoreSuggestions clears chatting" do
                  let
                        { chatting } = DT.fst <<< CIS.displayMoreSuggestions [] $ model
                              { chatting = Just 2
                              , suggestions = [ imUser, imUser ]
                              }
                  TUA.equal Nothing chatting

            TU.test "displayMoreSuggestions sets suggesting to 0 if there is 1 or fewer new suggestions" do
                  let
                        { suggesting } = DT.fst <<< CIS.displayMoreSuggestions [ imUser ] $ model
                              { suggesting = 0
                              , suggestions = []
                              }
                  TUA.equal 0 suggesting

            TU.test "blockUser removes user from suggestions" do
                  let
                        { suggestions } = DT.fst <<< CIM.blockUser webSocket imUser.id $ model
                              { suggestions = [ imUser ]
                              }
                  TUA.equal [] suggestions

            TU.test "blockUser removes user from contacts" do
                  let
                        { contacts } = DT.fst <<< CIM.blockUser webSocket contact.user.id $ model
                              { contacts = [ contact ]
                              }
                  TUA.equal [] contacts

            TU.test "blockUser resets chatting" do
                  let
                        { chatting } = DT.fst <<< CIM.blockUser webSocket contact.user.id $ model
                              { contacts = [ contact ]
                              }
                  TUA.equal Nothing chatting

