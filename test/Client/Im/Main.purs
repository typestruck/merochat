module Test.Client.Im.Main where

import Prelude
import Shared.Availability
import Shared.Im.Types

import Client.Im.Main as CIM
import Client.Im.WebSocket.Events as CIWE
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple as DT
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
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
                              DT.fst <<< CIWE.receiveMessage webSocket true
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
                                                              , edited: false
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
                              DT.fst <<< CIWE.receiveMessage webSocket true
                                    ( NewIncomingMessage
                                            { date
                                            , id: newMessageID
                                            , content
                                            , recipientId: contact.user.id
                                            , senderId: 3
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
                              DT.fst <<< CIWE.receiveMessage webSocket true
                                    ( PayloadError
                                            { origin: OutgoingMessage
                                                    { id: 1
                                                    , userName : "test"
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
                        { contacts } = DT.fst <<< CIWE.receiveMessage webSocket true (ContactUnavailable { userId: contact.user.id, temporaryMessageId: Just 1 }) $ model
                              { contacts = [ contact { history = [ historyMessage ] } ]
                              , chatting = Nothing
                              }

                  TUA.equal [ contact { user { availability = Unavailable }, history = [ historyMessage { status = Errored } ] } ] contacts

            TU.test "receiveMessage removes blocker users from suggestions" do
                  let
                        { suggestions } = DT.fst <<< CIWE.receiveMessage webSocket true (ContactUnavailable { userId: contact.user.id, temporaryMessageId: Nothing }) $ model
                              { suggestions = [ contact.user ]
                              , chatting = Nothing
                              }

                  TUA.equal [] suggestions

            TU.test "receiveMessage syncs message to history" do
                  date ← liftEffect $ map DateTimeWrapper EN.nowDateTime
                  let
                        updatedModel /\ _ = CIWE.receiveMessage webSocket true
                              ( NewIncomingMessage
                                      { date
                                      , id: newMessageID
                                      , content
                                      , recipientId: contact.user.id
                                      , senderId: model.user.id
                                      }
                              )
                              model
                                    { contacts = [ contact ]
                                    , chatting = Nothing
                                    }
                  TUA.equal
                        ( Just
                                { status: Received
                                , id: newMessageID
                                , content
                                , sender: model.user.id
                                , edited: false
                                , recipient: contact.user.id
                                , date
                                }
                        ) $ getHistory updatedModel.contacts

            TU.test "receiveMessage adds message to history" do
                  date ← liftEffect $ map DateTimeWrapper EN.nowDateTime
                  let
                        { contacts } =
                              DT.fst <<< CIWE.receiveMessage webSocket true
                                    ( NewIncomingMessage
                                            { date
                                            , id: newMessageID
                                            , content
                                            , recipientId: model.user.id
                                            , senderId: contact.user.id
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
                                , recipient: model.user.id
                                , edited: false
                                , date
                                }
                        ) $ getHistory contacts

            TU.test "receiveMessage mark messages as read if coming from current chat" do
                  date ← liftEffect $ map DateTimeWrapper EN.nowDateTime
                  let
                        { contacts } =
                              DT.fst <<< CIWE.receiveMessage webSocket true
                                    ( NewIncomingMessage
                                            { id: newMessageID
                                            , recipientId: model.user.id
                                            , senderId: contactId
                                            , content
                                            , date
                                            }
                                    ) $ model
                                    { contacts = [ contact ]
                                    , chatting = Just contact.user.id
                                    , suggesting = Just 0
                                    }
                  TUA.equal [ Tuple newMessageID Read ] $ map (\({ id, status }) → Tuple id status) (contacts !@ 0).history

            TU.test "receiveMessage does not mark messages as read if window is not focused" do
                  date ← liftEffect $ map DateTimeWrapper EN.nowDateTime
                  let
                        { contacts } =
                              DT.fst <<< CIWE.receiveMessage webSocket false
                                    ( NewIncomingMessage
                                            { id: newMessageID
                                            , recipientId: model.user.id
                                            , senderId: anotherImUserId
                                            , content
                                            , date
                                            }
                                    ) $ model
                                    { contacts = [ contact ]
                                    , chatting = Just contact.user.id
                                    , suggesting = Just 0
                                    }
                  TUA.equal [ Tuple newMessageID Delivered ] $ map (\({ id, status }) → Tuple id status) (contacts !@ 0).history

            TU.test "checkMissedEvents finds first message with sent status" do
                  let
                        dummyContact = contact { user = imUser, history = [ historyMessage, historyMessage { status = Sent, id = 25 } ] }
                        anotherDummyContact = dummyContact { history = [ historyMessage, historyMessage ] }
                        msg = CIM.checkMessagesFrom [ dummyContact, anotherDummyContact ] imUserId
                  TUA.equal (Just 25) msg

            TU.test "checkMissedEvents finds last sent message" do
                  let
                        dummyContact = contact { user = imUser, history = [ historyMessage, historyMessage ] }
                        anotherDummyContact = dummyContact { history = [ historyMessage { id = 25, sender = imUserId }, historyMessage { sender = anotherImUserId, id = 2 } ] }
                        msg = CIM.checkMessagesFrom [ dummyContact, anotherDummyContact ] imUserId
                  TUA.equal (Just 25) msg

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
