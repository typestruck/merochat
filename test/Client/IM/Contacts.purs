module Test.Client.IM.Contacts where

import Prelude
import Shared.Types

import Client.IM.Contacts as CICN
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple as DT
import Effect.Now as EN
import Effect.Unsafe as EU

import Shared.Unsafe ((!@))
import Test.Client.Model (anotherIMUserID, contact, imUser, webSocket)
import Test.Client.Model as TCM
import Test.Unit (TestSuite)
import Test.Unit as TU
import Test.Unit.Assert as TUA

tests :: TestSuite
tests = do
      TU.suite "im contacts update" do
            TU.test "resumeChat sets chatting" do
                  let m@{ chatting } = DT.fst <<< CICN.resumeChat anotherIMUserID $ model {
                        chatting = Nothing,
                        contacts = [contact { user = imUser }, contact]
                  }
                  TUA.equal (Just 1) chatting

                  let { chatting } = DT.fst $ CICN.resumeChat anotherContactID m
                  TUA.equal (Just 0) chatting

            TU.test "resumeChat marks as read" do
                  TUA.equal 2 1

            TU.test "displayHistory unchecks shouldFetchChatHistory" do
                  TUA.equal 2 1

            TU.test "markRead sets recieved messages as read" do
                  let { contacts } = DT.fst <<< CICN.markRead webSocket $ model {
                        chatting = Just 1
                  }
                  TUA.equal [Tuple 1 Read, Tuple 2 Received, Tuple 3 Read] <<< map (\( { id, status}) -> Tuple id status) $ (contacts !@ 1).history

            TU.test "displayContacts shows next page" do
                  let { contacts } = DT.fst <<< CICN.displayContacts [contact] $ model {
                        contacts = []
                  }
                  TUA.equal contacts [contact]

model :: IMModel
model = TCM.model  {
      contacts = [contact, anotherContact]
}

anotherContactID :: PrimaryKey
anotherContactID = 23

anotherContact :: Contact
anotherContact = contact {
      user = imUser { id = anotherContactID },
      history = [{
            id: 1,
            status: Received,
            sender: 32,
            recipient: imUser.id,
            content: "1",
            date: EU.unsafePerformEffect $ map DateTimeWrapper EN.nowDateTime
      }, {
            id:  2,
            status: Received,
            sender: imUser.id,
            recipient:  32,
            content: "2",
            date: EU.unsafePerformEffect $ map DateTimeWrapper EN.nowDateTime
      },{
            id:  3,
            status: Received,
            sender:  32,
            recipient: imUser.id,
            content: "3",
            date: EU.unsafePerformEffect $ map DateTimeWrapper EN.nowDateTime
      }]
}