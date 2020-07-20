module Test.Client.IM.Chat where

import Client.Common.Types
import Prelude
import Shared.IM.Types
import Shared.Types

import Client.IM.Chat as CIC
import Data.Array ((!!), (:))
import Data.Array as DA
import Data.Either (Either(..))
import Data.Int53 as DI
import Data.Maybe (Maybe(..))
import Data.Newtype as DN
import Data.Tuple (Tuple(..))
import Data.Tuple as DT
import Debug.Trace (spy)
import Effect.Class (liftEffect)
import Effect.Now as EN
import Partial.Unsafe as PU
import Shared.Newtype as SN
import Shared.PrimaryKey as SP
import Shared.Unsafe ((!@))
import Shared.Unsafe as SN
import Test.Client.Model (anotherIMUser, anotherIMUserID, contact, contactID, historyMessage, imUser, imUserID, model, suggestion)
import Test.Client.Model as TCM
import Test.Unit (TestSuite)
import Test.Unit as TU
import Test.Unit.Assert as TUA
import Unsafe.Coerce as UC
import Web.Socket.WebSocket (WebSocket)

tests :: TestSuite
tests = do
      TU.suite "im chat update" do
            let content = "test"

            TU.test "sendMessage bumps temporary id" do
                  date <- liftEffect $ map MDateTime EN.nowDateTime
                  let m@(IMModel {temporaryID}) = DT.fst $ CIC.sendMessage content date model
                  TUA.equal (SP.fromInt 1) temporaryID

                  let IMModel {temporaryID} = DT.fst $ CIC.sendMessage content date m
                  TUA.equal (SP.fromInt 2) temporaryID

            TU.test "sendMessage adds message to history" do
                  date <- liftEffect $ map MDateTime EN.nowDateTime
                  let IMModel { user: IMUser { id: userID }, contacts, chatting } = DT.fst $ CIC.sendMessage content date model
                      Contact user = SN.fromJust "test" do
                        index <- chatting
                        contacts !! index

                  TUA.equal [HistoryMessage {
                        date: _.date $ DN.unwrap (user.history !@ 0),
                        recipient: _.id $ DN.unwrap user.user,
                        status: Unread,
                        id: SP.fromInt 1,
                        content,
                        sender: userID
                  }] user.history

            let recipientMessage = SN.updateHistoryMessage historyMessage $ _ {
                  sender = anotherIMUserID,
                  recipient = imUserID
            }

            TU.test "makeTurn calculate turn" do
                  let contact' = SN.updateContact contact $ _ {
                        history = [historyMessage, recipientMessage, historyMessage]
                  }
                      turn = Just $ Turn {
                            chatAge: 0.0,
                            recipientStats: Stats {
                                  characters: 4.0,
                                  interest: 1.0
                            },
                            replyDelay: 0.0,
                            senderStats: Stats {
                                  characters: 4.0,
                                  interest: 1.0
                            }
                        }
                  TUA.equal turn $ CIC.makeTurn contact' imUserID

            TU.test "makeTurn don't calculate turn for recipient " do
                  TUA.equal Nothing <<< CIC.makeTurn contact $ SP.fromInt 90000

            TU.test "makeTurn don't calculate turn if last message isn't from the sender" do
                  let contact' = SN.updateContact contact $ _ {
                        history = [historyMessage, recipientMessage, historyMessage, recipientMessage]
                  }
                  TUA.equal Nothing $ CIC.makeTurn contact' contactID

            let IMModel { suggestions : modelSuggestions } = model

            TU.test "startChat adds new contact from suggestion" do
                  let model' = SN.updateModel model $ _ {
                              suggestions = suggestion : modelSuggestions,
                              chatting = Nothing,
                              suggesting = Just 0
                        }
                      IMModel { contacts } = DT.fst $ CIC.startChat model' content
                  TUA.equal ( _.user <<< DN.unwrap <$> DA.head contacts) $ Just suggestion

            TU.test "startChat resets suggesting" do
                  let model' = SN.updateModel model $ _ {
                              suggestions = suggestion : modelSuggestions,
                              chatting = Nothing,
                              suggesting = Just 0
                        }
                      IMModel { suggesting } = DT.fst $ CIC.startChat model' content
                  TUA.equal Nothing suggesting

            TU.test "startChat sets chatting to 0" do
                  let model' = SN.updateModel model $ _ {
                              suggestions = suggestion : modelSuggestions,
                              chatting = Nothing,
                              suggesting = Just 0
                        }
                      IMModel { chatting } = DT.fst $ CIC.startChat model' content
                  TUA.equal (Just 0) chatting

            let IMUser { id: recipientID } = imUser
                messageID = SP.fromInt 1
                newMessageID = SP.fromInt 101

            TU.test "receiveMessage substitutes temporary id" do
                  date <- liftEffect $ map MDateTime EN.nowDateTime
                  let IMModel {contacts} = DT.fst <<< CIC.receiveMessage true (SN.updateModel model $ _ {
                        contacts = [SN.updateContact contact $ _ {
                              history = [HistoryMessage {
                                    status: Unread,
                                    date,
                                    id: messageID,
                                    recipient: recipientID,
                                    sender: anotherIMUserID,
                                    content
                              }]
                        }]
                        }) $ Received {
                              previousID: messageID,
                              id : newMessageID
                        }
                  TUA.equal (Just newMessageID) $ getMessageID contacts

            TU.test "receiveMessage adds message to history" do
                  date <- liftEffect $ map MDateTime EN.nowDateTime
                  let IMModel {contacts} = DT.fst <<< CIC.receiveMessage true (SN.updateModel model $ _ {
                        contacts = [contact],
                        chatting = Nothing
                        }) $ ClientMessage {
                              date,
                              id: newMessageID,
                              content,
                              user: Right anotherIMUserID
                        }
                  TUA.equal (Just $ HistoryMessage {
                        status: Unread,
                        id: newMessageID,
                        content,
                        sender: anotherIMUserID,
                        recipient: recipientID,
                        date
                  }) $ getHistory contacts

            TU.test "receiveMessage adds contact if new" do
                  date <- liftEffect $ map MDateTime EN.nowDateTime
                  let IMModel { contacts } = DT.fst <<< CIC.receiveMessage true (SN.updateModel model $ _ {
                        contacts = [],
                        chatting = Nothing
                        }) $ ClientMessage {
                              date,
                              id: newMessageID,
                              content,
                              user: Left anotherIMUser
                        }
                  TUA.equal (_.history $ DN.unwrap (contacts !@ 0)) [
                        HistoryMessage {
                              status: Unread,
                              id: newMessageID,
                              sender: anotherIMUserID,
                              recipient: recipientID,
                              content,
                              date
                        }
                  ]

            TU.test "receiveMessage set chatting if message comes from current suggestion" do
                  date <- liftEffect $ map MDateTime EN.nowDateTime
                  let model' = SN.updateModel model $ _ {
                        contacts = [],
                        chatting = Nothing,
                        suggesting = Just 0,
                        suggestions = [anotherIMUser]
                        }
                      IMModel { contacts, chatting } = DT.fst <<< CIC.receiveMessage true model' $ ClientMessage {
                              id: newMessageID,
                              user: Left anotherIMUser,
                              content,
                              date
                        }
                      suggestionToContact = Just $ SN.updateContact contact $ _ {
                        chatStarter = anotherIMUserID,
                        user = anotherIMUser,
                        history = [HistoryMessage
                        {
                              status: Read,
                              id: newMessageID,
                              sender: anotherIMUserID,
                              recipient: recipientID,
                              date,
                              content
                        }]
                  }
                  TUA.equal suggestionToContact $ DA.head contacts
                  TUA.equal chatting $ Just 0

            TU.test "receiveMessage mark messages as read if coming from current chat" do
                  date <- liftEffect $ map MDateTime EN.nowDateTime
                  let IMModel { contacts } = DT.fst <<< CIC.receiveMessage true (SN.updateModel model $ _ {
                        contacts = [contact],
                        chatting = Just 0,
                        suggesting = Nothing
                        }) $ ClientMessage {
                              id: newMessageID,
                              user: Left anotherIMUser,
                              content,
                              date
                        }
                  TUA.equal [Tuple newMessageID Read] <<< map (\(HistoryMessage { id, status}) -> Tuple id status) <<< _.history $ DN.unwrap (contacts !@ 0)

            TU.test "receiveMessage marks messages as read if window is not focused" do
                  date <- liftEffect $ map MDateTime EN.nowDateTime
                  let IMModel { contacts } = DT.fst <<< CIC.receiveMessage false (SN.updateModel model $ _ {
                        contacts = [contact],
                        chatting = Just 0,
                        suggesting = Nothing
                        }) $ ClientMessage {
                              id: newMessageID,
                              user: Left anotherIMUser,
                              content,
                              date
                        }
                  TUA.equal [Tuple newMessageID Unread] <<< map (\(HistoryMessage { id, status}) -> Tuple id status) <<< _.history $ DN.unwrap (contacts !@ 0)

      where getHistory contacts = do
                  Contact { history } <- DA.head contacts
                  DA.head history
            getMessageID contacts = do
                  HistoryMessage { id } <- getHistory contacts
                  pure id