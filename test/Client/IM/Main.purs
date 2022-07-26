module Test.Client.IM.Main where

import Prelude
import Shared.ContentType
import Shared.Experiments.Types
import Shared.IM.Types
import Shared.User

import Client.IM.Main as CIM
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple as DT
import Effect.Class (liftEffect)
import Effect.Now as EN
import Shared.DateTime (DateTimeWrapper(..))
import Shared.Experiments.Impersonation (batman)
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
                                                              , recipient: recipientID
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
                                            , experimenting: Nothing
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
                  date ← liftEffect $ map DateTimeWrapper EN.nowDateTime
                  let
                        { contacts } =
                              DT.fst <<< CIM.receiveMessage webSocket true
                                    ( PayloadError
                                            { origin: OutgoingMessage
                                                    { id: 1
                                                    , userId: contact.user.id
                                                    , content: Text content
                                                    , experimenting: Nothing
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
                                            , experimenting: Nothing
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
                                , recipient: recipientID
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
                                            , experimenting: Nothing
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

            TU.test "receiveMessage ignores impersonation messages that dont match current" do
                  date ← liftEffect $ map DateTimeWrapper EN.nowDateTime
                  let
                        { contacts } =
                              DT.fst <<< CIM.receiveMessage webSocket true
                                    ( NewIncomingMessage
                                            { date
                                            , id: newMessageID
                                            , content
                                            , experimenting: Just $ ImpersonationPayload { id: batman.id + 1, sender: false }
                                            , userId: contact.user.id
                                            }
                                    ) $ model
                                    { contacts = [ contact ]
                                    , chatting = Nothing
                                    , experimenting = Just <<< Impersonation $ Just batman
                                    }
                  TUA.equal Nothing $ getHistory contacts

            TU.test "receiveMessage does not mark messages as read if window is not focused" do
                  date ← liftEffect $ map DateTimeWrapper EN.nowDateTime
                  let
                        { contacts } =
                              DT.fst <<< CIM.receiveMessage webSocket false
                                    ( NewIncomingMessage
                                            { id: newMessageID
                                            , experimenting: Nothing
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
                        { lastSentMessageID } = CIM.findLastMessages [ dummyContact, anotherDummyContact ] imUserId
                  TUA.equal (Just 25) lastSentMessageID

            TU.test "checkMissedEvents finds last received message id" do
                  let
                        dummyContact = contact { user = imUser, history = [ historyMessage, historyMessage ] }
                        anotherDummyContact = dummyContact { history = [ historyMessage { sender = anotherImUserId, id = 25 }, historyMessage { sender = anotherImUserId, id = 2 } ] }
                        { lastReceivedMessageID } = CIM.findLastMessages [ dummyContact, anotherDummyContact ] imUserId
                  TUA.equal (Just 25) lastReceivedMessageID

      where
      getHistory contacts = do
            { history } ← DA.head contacts
            DA.head history
      getMessageID contacts = do
            { id } ← getHistory contacts
            pure id

      content = "test"
      { id: recipientID } = imUser
      messageId = 1
      newMessageID = 101
      { suggestions: modelSuggestions } = model

      recipientMessage = historyMessage
            { sender = anotherImUserId
            , recipient = imUserId
            }
      image = "base64"
      caption = "caption"