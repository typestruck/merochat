module Test.Client.Im.Contacts where

import Prelude
import Shared.DateTime
import Shared.Im.Types
import Shared.User

import Client.Im.Contacts as CICN
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple as DT
import Data.Tuple.Nested ((/\))
import Effect.Now as EN
import Effect.Unsafe as EU
import Shared.Unsafe ((!@))
import Test.Client.Model (contact, historyMessage, imUser, imUserId, model, webSocket)
import Test.Unit (TestSuite)
import Test.Unit as TU
import Test.Unit.Assert as TUA

tests ∷ TestSuite
tests = do
      TU.suite "im contacts update" do
            TU.test "resumeChat sets chatting" do
                  let
                        m@{ chatting } = DT.fst <<< CICN.resumeChat anotherContactId $ model
                              { chatting = Nothing
                              , contacts = [ contact { user = imUser }, anotherContact ]
                              }
                  TUA.equal (Just anotherContact.user.id) chatting

                  let { chatting } = DT.fst $ CICN.resumeChat imUserId m
                  TUA.equal (Just imUser.id) chatting

            TU.test "setReadStatus sets received messages as read" do
                  let
                        { contacts } = DT.fst <<< CICN.setReadStatus Nothing webSocket $ model
                              { chatting = Just anotherContact.user.id
                              , contacts = [ contact, anotherContact ]
                              }
                  TUA.equal [ Tuple 1 Read, Tuple 2 Received, Tuple 3 Read ] <<< map (\({ id, status }) → Tuple id status) $ (contacts !@ 1).history

            TU.test "setDeliveredStatus sets received messages as unread" do
                  let
                        { contacts } = DT.fst <<< CICN.setDeliveredStatus webSocket $ model
                              { user { id = 4 }
                              , chatting = Just anotherContact.user.id
                              , contacts =
                                      [ contact
                                              { history =
                                                      [ historyMessage { status = Received, recipient = 4 }
                                                      , historyMessage { status = Received, recipient = 4 }
                                                      ]
                                              }
                                      , anotherContact
                                              { history =
                                                      [ historyMessage { status = Received }
                                                      , historyMessage { status = Read }
                                                      ]
                                              }
                                      ]
                              }
                  TUA.equal [ Delivered, Delivered, Received, Read ] $ DA.concatMap (map _.status <<< _.history) contacts

            TU.test "displayContacts shows next page" do
                  let
                        { contacts } = DT.fst <<< CICN.displayContacts [ contact ] $ model
                              { contacts = []
                              }
                  TUA.equal [ contact ] contacts

            TU.test "displayContacts filters duplicates" do
                  let
                        { contacts } = DT.fst <<< CICN.displayContacts [ contact ] $ model
                              { contacts = [ contact ]
                              }
                  TUA.equal [ contact ] contacts

            -- TU.test "displayMissedContacts adds missed messages" do
            --       let
            --             updatedModel /\ _ = CICN.displayMissedContacts { missedMessages: [ historyMessage ] } model
            --                   { contacts = [ contact { history = [] } ]
            --                   }
            --       TUA.equal 1 $ DA.length updatedModel.contacts
            --       TUA.equal [ historyMessage ] (updatedModel.contacts !@ 0).history

            -- TU.test "displayMissedContacts syncs messages status" do
            --       let
            --             updatedModel /\ _ = CICN.displayMissedContacts { missedMessages: [ historyMessage { status = Read } ] } model
            --                   { contacts = [ contact { history = [ historyMessage { status = Sent } ] } ]
            --                   }
            --       TUA.equal 1 $ DA.length updatedModel.contacts
            --       TUA.equal [ Read ] $ map _.status (updatedModel.contacts !@ 0).history

            -- TU.test "displayMissedContacts ignores messages from new contacts" do
            --       let
            --             updatedModel /\ _ = CICN.displayMissedContacts { missedMessages: [ historyMessage ] } model
            --                   { contacts = []
            --                   }
            --       TUA.equal 0 $ DA.length updatedModel.contacts

            TU.test "deleteChat removes deleted contact" do
                  let
                        { contacts } = DT.fst <<< CICN.deleteChat anotherContactId $ model
                              { contacts = [ contact, anotherContact ]
                              }
                  TUA.equal [ contact ] contacts

      where
      anotherContactId = imUser.id + 1
      anotherContact = contact
            { user = imUser { id = anotherContactId }
            , history =
                    [ { id: 1
                      , status: Received
                      , sender: 32
                      , recipient: imUser.id
                      , edited: false
                      , content: "1"
                      , date: EU.unsafePerformEffect $ map DateTimeWrapper EN.nowDateTime
                      }
                    , { id: 2
                      , status: Received
                      , sender: imUser.id
                      , recipient: 32
                      , content: "2"
                      , edited: false
                      , date: EU.unsafePerformEffect $ map DateTimeWrapper EN.nowDateTime
                      }
                    , { id: 3
                      , status: Received
                      , sender: 32
                      , recipient: imUser.id
                      , edited: false
                      , content: "3"
                      , date: EU.unsafePerformEffect $ map DateTimeWrapper EN.nowDateTime
                      }
                    ]
            }
