module Test.Client.Im.History where

import Prelude

import Client.Im.History as CIH
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple as DT
import Effect.Now as EN
import Effect.Unsafe as EU
import Shared.Unsafe ((!@))
import Test.Client.Model (contact, contactId, historyMessage, imUser, imUserId, model, webSocket)
import Test.Unit (TestSuite)
import Test.Unit as TU
import Test.Unit.Assert as TUA

tests ∷ TestSuite
tests = do
      TU.suite "im history update" do
            TU.test "displayHistory sets freeToFetchChatHistory" do
                  let
                        { freeToFetchChatHistory } = DT.fst <<< CIH.displayHistory contact.user.id [ historyMessage ] $ model
                              { chatting = Just contact.user.id
                              , contacts = [ contact ]
                              , freeToFetchChatHistory = false
                              }
                  TUA.equal true freeToFetchChatHistory

            TU.test "displayHistory unsets shouldFetchChatHistory" do
                  let
                        { contacts } = DT.fst <<< CIH.displayHistory contact.user.id [ historyMessage ] $ model
                              { chatting = Just contact.user.id
                              , contacts =
                                      [ contact
                                              { shouldFetchChatHistory = true
                                              }
                                      ]
                              , freeToFetchChatHistory = false
                              }
                  TUA.equal [ contact { history = [ historyMessage ], shouldFetchChatHistory = false } ] contacts