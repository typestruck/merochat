module Test.Client.Im.Main where

import Prelude
import Shared.Experiments.Types
import Shared.Im.Types
import Shared.User

import Client.Im.Main as CIM
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple as DT
import Effect.Class (liftEffect)
import Shared.Availability
import Effect.Now as EN
import Shared.DateTime (DateTimeWrapper(..))
import Shared.ResponseError (DatabaseError(..))
import Shared.Unsafe ((!@))
import Test.Client.Model (anotherImUserId, contact, contactId, historyMessage, imUser, imUserId, model, webSocket)
import Test.Unit (TestSuite)
import Test.Unit as TU
import Test.Unit.Assert as TUA

tests ∷ TestSuite
tests = do
      TU.suite "socket operations" do
            TU.test "receiveMessage substitutes temporary id" do
                  date ← liftEffect $ map DateTimeWrapper EN.nowDateTime
                  let
                        { contacts } =
                              DT.fst <<< CIM.receiveMessage webSocket true
                                    ( ServerReceivedMessage
                                            { previousId: messageId
                                            , id: newMessageID
                                            , userId: contactId
                                            }
                                    ) $ model
                                    { contacts =
                                            [ contact
                                                    { history =
                                                            [ { status: Received
                                                              , date
                                                              , id: messageId
                                                              , recipient: recipientId
                                                              , sender: anotherImUserId
                                                              , content
                                                              }
                                                            ]
                                                    }
                                            ]
                                    }
                  TUA.equal (Just newMessageID) $ getMessageID contacts

            TU.test "receiveMessage removes user from suggestions" do
                  date ← liftEffect $ map DateTimeWrapper EN.nowDateTime
                  let
                        { suggestions } =
                              DT.fst <<< CIM.receiveMessage webSocket true
                                    ( NewIncomingMessage
                                            { date
                                            , id: newMessageID
                                            , content
                                            , userId: contact.user.id
                                            }
                                    ) $ model
                                    { suggestions = [ contact.user ]
                                    , contacts = [ contact ]
                                    , chatting = Nothing
                                    }
                  TUA.equal [] suggestions

            TU.test "receiveMessage marks deleted users as unavailable" do
                  let
                        { contacts } =
                              DT.fst <<< CIM.receiveMessage webSocket true
                                    ( PayloadError
                                            { origin: OutgoingMessage
                                                    { id: 1
                                                    , userId: contact.user.id
                                                    , content: Text content
                                                    , turn: Nothing
                                                    }
                                            , context: Just MissingForeignKey
                                            }
                                    ) $ model
                                    { contacts = [ contact ]
                                    , chatting = Nothing
                                    }

                  TUA.equal [ contact { user { availability = Unavailable } } ] contacts

            TU.test "receiveMessage marks blocker users as unavailable" do
                  let
                        { contacts } = DT.fst <<< CIM.receiveMessage webSocket true (ContactUnavailable { userId: contact.user.id, temporaryMessageId: Just 1 }) $ model
                              { contacts = [ contact { history = [ historyMessage ] } ]
                              , chatting = Nothing
                              }

                  TUA.equal [ contact { user { availability = Unavailable }, history = [ historyMessage { status = Errored } ] } ] contacts

            TU.test "receiveMessage removes blocker users from suggestions" do
                  let
                        { suggestions } = DT.fst <<< CIM.receiveMessage webSocket true (ContactUnavailable { userId: contact.user.id, temporaryMessageId: Nothing }) $ model
                              { suggestions = [ contact.user ]
                              , chatting = Nothing
                              }

                  TUA.equal [] suggestions

            TU.test "receiveMessage adds message to history" do
                  date ← liftEffect $ map DateTimeWrapper EN.nowDateTime
                  let
                        { contacts } =
                              DT.fst <<< CIM.receiveMessage webSocket true
                                    ( NewIncomingMessage
                                            { date
                                            , id: newMessageID
                                            , content
                                            , userId: contact.user.id
                                            }
                                    ) $ model
                                    { contacts = [ contact ]
                                    , chatting = Nothing
                                    }
                  TUA.equal
                        ( Just
                                { status: Delivered
                                , id: newMessageID
                                , content
                                , sender: contact.user.id
                                , recipient: recipientId
                                , date
                                }
                        ) $ getHistory contacts

            TU.test "receiveMessage mark messages as read if coming from current chat" do
                  date ← liftEffect $ map DateTimeWrapper EN.nowDateTime
                  let
                        { contacts } =
                              DT.fst <<< CIM.receiveMessage webSocket true
                                    ( NewIncomingMessage
                                            { id: newMessageID
                                            , userId: contactId
                                            , content
                                            , date
                                            }
                                    ) $ model
                                    { contacts = [ contact ]
                                    , chatting = Just 0
                                    , suggesting = Nothing
                                    }
                  TUA.equal [ Tuple newMessageID Read ] $ map (\({ id, status }) → Tuple id status) (contacts !@ 0).history

            TU.test "receiveMessage does not mark messages as read if window is not focused" do
                  date ← liftEffect $ map DateTimeWrapper EN.nowDateTime
                  let
                        { contacts } =
                              DT.fst <<< CIM.receiveMessage webSocket false
                                    ( NewIncomingMessage
                                            { id: newMessageID
                                            , userId: anotherImUserId
                                            , content
                                            , date
                                            }
                                    ) $ model
                                    { contacts = [ contact ]
                                    , chatting = Just 0
                                    , suggesting = Nothing
                                    }
                  TUA.equal [ Tuple newMessageID Delivered ] $ map (\({ id, status }) → Tuple id status) (contacts !@ 0).history

            TU.test "checkMissedEvents finds last sent message id" do
                  let
                        dummyContact = contact { user = imUser, history = [ historyMessage, historyMessage ] }
                        anotherDummyContact = dummyContact { history = [ historyMessage, historyMessage { id = 25 } ] }
                        { lastSentMessageId } = CIM.findLastMessages [ dummyContact, anotherDummyContact ] imUserId
                  TUA.equal (Just 25) lastSentMessageId

            TU.test "checkMissedEvents finds last received message id" do
                  let
                        dummyContact = contact { user = imUser, history = [ historyMessage, historyMessage ] }
                        anotherDummyContact = dummyContact { history = [ historyMessage { sender = anotherImUserId, id = 25 }, historyMessage { sender = anotherImUserId, id = 2 } ] }
                        { lastReceivedMessageId } = CIM.findLastMessages [ dummyContact, anotherDummyContact ] imUserId
                  TUA.equal (Just 25) lastReceivedMessageId

      where
      getHistory contacts = do
            { history } ← DA.head contacts
            DA.head history
      getMessageID contacts = do
            { id } ← getHistory contacts
            pure id

      content = "test"
      { id: recipientId } = imUser
      messageId = 1
      newMessageID = 101
