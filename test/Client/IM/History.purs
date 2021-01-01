module Test.Client.IM.History where

import Prelude
import Shared.Types

import Client.IM.History as CIH
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple as DT
import Effect.Now as EN
import Effect.Unsafe as EU
import Shared.Unsafe ((!@))
import Test.Client.Model (contact, contactID, historyMessage, imUser, imUserID, model, webSocket)
import Test.Unit (TestSuite)
import Test.Unit as TU
import Test.Unit.Assert as TUA

tests :: TestSuite
tests = do
      TU.suite "im history update" do
            TU.test "displayHistory sets freeToFetchChatHistory" do
                  let m@{ freeToFetchChatHistory } = DT.fst <<< CIH.displayHistory [historyMessage] $ model {
                        chatting = Just 0,
                        contacts = [contact],
                        freeToFetchChatHistory = false
                  }
                  TUA.equal true freeToFetchChatHistory

            TU.test "displayHistory unsets shouldFetchChatHistory" do
                  let m@{ contacts } = DT.fst <<< CIH.displayHistory [historyMessage] $ model {
                        chatting = Just 0,
                        contacts = [contact {
                              shouldFetchChatHistory = true
                        }],
                        freeToFetchChatHistory = false
                  }
                  TUA.equal [contact { history = [historyMessage], shouldFetchChatHistory = false}] contacts