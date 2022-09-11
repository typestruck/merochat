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
import Effect.Now as EN
import Effect.Unsafe as EU
import Shared.Experiments.Impersonation (batman)
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
                        m@{ chatting } = DT.fst <<< CICN.resumeChat anotherContactId Nothing $ model
                              { chatting = Nothing
                              , contacts = [ contact { user = imUser }, anotherContact ]
                              }
                  TUA.equal (Just 1) chatting

                  let { chatting } = DT.fst $ CICN.resumeChat imUserId Nothing m
                  TUA.equal (Just 0) chatting

            TU.test "resumeChat finds impersonation contact" do
                  let
                        { chatting } = DT.fst <<< CICN.resumeChat imUser.id (Just batman.id) $ model
                              { chatting = Nothing
                              , contacts = [ contact { user = imUser }, anotherContact { user = imUser, impersonating = Just batman.id } ]
                              }
                  TUA.equal (Just 1) chatting

            TU.test "markRead sets received messages as read" do
                  let
                        { contacts } = DT.fst <<< CICN.markRead webSocket $ model
                              { chatting = Just 1
                              , contacts = [ contact, anotherContact ]
                              }
                  TUA.equal [ Tuple 1 Read, Tuple 2 Received, Tuple 3 Read ] <<< map (\({ id, status }) → Tuple id status) $ (contacts !@ 1).history

            TU.test "markDelivered sets received messages as unread" do
                  let
                        { contacts } = DT.fst <<< CICN.markDelivered webSocket $ model
                              { user { id = 4 }
                              , chatting = Just 1
                              , contacts =
                                      [ contact
                                              { history =
                                                      [ historyMessage { status = Received, recipient = 4  }
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

            TU.test "resumeMissedEvents adds missed contacts" do
                  let
                        { contacts } = DT.fst <<< CICN.resumeMissedEvents { contacts: [ contact ], messageIds: [] } $ model
                              { contacts = []
                              }
                  TUA.equal [ contact ] contacts

            TU.test "resumeMissedEvents adds missed messages" do
                  let
                        updated = [ contact { history = [ historyMessage ] } ]
                        { contacts } = DT.fst <<< CICN.resumeMissedEvents { contacts: updated, messageIds: [] } $ model
                              { contacts = [ contact { history = [] } ]
                              }
                  TUA.equal updated contacts

            TU.test "resumeMissedEvents sets successful messages" do
                  let
                        temporaryId = 1
                        newID = 10
                        existing =
                              [ contact
                                      { history =
                                              [ historyMessage
                                                      { status = Sent
                                                      , id = temporaryId
                                                      }
                                              ]
                                      }
                              ]
                        { contacts } = DT.fst <<< CICN.resumeMissedEvents { contacts: [], messageIds: [ { id: newID, temporaryId } ] } $ model
                              { contacts = existing
                              }
                        updated =
                              [ contact
                                      { history =
                                              [ historyMessage
                                                      { status = Received
                                                      , id = newID
                                                      }
                                              ]
                                      }
                              ]
                  TUA.equal updated contacts

            TU.test "resumeMissedEvents sets errored messages" do
                  let
                        temporaryId = 1
                        existing =
                              [ contact
                                      { history =
                                              [ historyMessage
                                                      { status = Sent
                                                      , id = temporaryId
                                                      }
                                              ]
                                      }
                              ]
                        { contacts } = DT.fst <<< CICN.resumeMissedEvents { contacts: [], messageIds: [] } $ model
                              { contacts = existing
                              }
                        updated =
                              [ contact
                                      { history =
                                              [ historyMessage
                                                      { status = Errored
                                                      }
                                              ]
                                      }
                              ]
                  TUA.equal updated contacts

            TU.test "deleteChat removes deleted contact" do
                  let
                        { contacts } = DT.fst <<< CICN.deleteChat (Tuple anotherContactId Nothing) $ model
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
                      , content: "1"
                      , date: EU.unsafePerformEffect $ map DateTimeWrapper EN.nowDateTime
                      }
                    , { id: 2
                      , status: Received
                      , sender: imUser.id
                      , recipient: 32
                      , content: "2"
                      , date: EU.unsafePerformEffect $ map DateTimeWrapper EN.nowDateTime
                      }
                    , { id: 3
                      , status: Received
                      , sender: 32
                      , recipient: imUser.id
                      , content: "3"
                      , date: EU.unsafePerformEffect $ map DateTimeWrapper EN.nowDateTime
                      }
                    ]
            }
