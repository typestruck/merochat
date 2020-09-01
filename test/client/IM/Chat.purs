module Test.Client.IM.Chat where

import Prelude
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
import Test.Client.Model (anotherIMUser, anotherIMUserID, contact, contactID, historyMessage, imUser, imUserID, model, suggestion, webSocket)
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
                  date <- liftEffect $ map DateTimeWrapper EN.nowDateTime
                  let m@{ temporaryID } = DT.fst $ CIC.sendMessage webSocket date model
                  TUA.equal (SP.fromInt 1) temporaryID

                  let { temporaryID } = DT.fst $ CIC.sendMessage webSocket date m
                  TUA.equal (SP.fromInt 2) temporaryID

            TU.test "sendMessage adds message to history" do
                  date <- liftEffect $ map DateTimeWrapper EN.nowDateTime
                  let   { user: { id: userID }, contacts, chatting } = DT.fst $ CIC.sendMessage webSocket date model
                        user = SN.fromJust do
                              index <- chatting
                              contacts !! index

                  TUA.equal [ {
                        date: (user.history !@ 0).date,
                        recipient: user.user.id,
                        status: Unread,
                        id: SP.fromInt 1,
                        content,
                        sender: userID
                  }] user.history

            let recipientMessage = historyMessage {
                  sender = anotherIMUserID,
                  recipient = imUserID
            }

            TU.test "makeTurn calculate turn" do
                  let   contact' = contact {
                              history = [recipientMessage]
                        }
                        turn = Just {
                              chatAge: 0.0,
                              recipientStats: {
                                    characters: 4.0,
                                    interest: 1.0
                              },
                              replyDelay: 0.0,
                              senderStats: {
                                    characters: 4.0,
                                    interest: 1.0
                              }
                        }
                  TUA.equal turn $ CIC.makeTurn contact' imUserID

            TU.test "makeTurn don't calculate turn for recipient " do
                  TUA.equal Nothing <<< CIC.makeTurn contact $ SP.fromInt 90000

            TU.test "makeTurn don't calculate turn if last message isn't from the sender" do
                  let contact' = contact {
                        history = [recipientMessage, recipientMessage]
                  }
                  TUA.equal Nothing $ CIC.makeTurn contact' contactID

            let { suggestions : modelSuggestions } = model

            TU.test "beforeSendMessage adds new contact from suggestion" do
                  let   model' = model {
                              suggestions = suggestion : modelSuggestions,
                              chatting = Nothing,
                              suggesting = Just 0
                        }
                        { contacts } = DT.fst $ CIC.beforeSendMessage true content model'
                  TUA.equal ( _.user <$> DA.head contacts) $ Just suggestion

            TU.test "beforeSendMessage resets suggesting" do
                  let   model' = model {
                              suggestions = suggestion : modelSuggestions,
                              chatting = Nothing,
                              suggesting = Just 0
                        }
                        { suggesting } = DT.fst $ CIC.beforeSendMessage true content model'
                  TUA.equal Nothing suggesting

            TU.test "beforeSendMessage sets chatting to 0" do
                  let   model' = model {
                              suggestions = suggestion : modelSuggestions,
                              chatting = Nothing,
                              suggesting = Just 0
                        }
                        { chatting } = DT.fst $ CIC.beforeSendMessage true content model'
                  TUA.equal (Just 0) chatting

            let { id: recipientID } = imUser
                messageID = SP.fromInt 1
                newMessageID = SP.fromInt 101

            TU.test "receiveMessage substitutes temporary id" do
                  date <- liftEffect $ map DateTimeWrapper EN.nowDateTime
                  let   {contacts} = DT.fst <<< CIC.receiveMessage webSocket true (Received {
                              previousID: messageID,
                              id : newMessageID,
                              userID: contactID
                        }) $ model {
                              contacts = [contact {
                                    history = [ {
                                          status: Unread,
                                          date,
                                          id: messageID,
                                          recipient: recipientID,
                                          sender: anotherIMUserID,
                                          content
                                    }]
                              }]
                        }
                  TUA.equal (Just newMessageID) $ getMessageID contacts

            TU.test "receiveMessage adds message to history" do
                  date <- liftEffect $ map DateTimeWrapper EN.nowDateTime
                  let   {contacts} = DT.fst <<< CIC.receiveMessage webSocket true (ClientMessage {
                              date,
                              id: newMessageID,
                              content,
                              userID: anotherIMUserID
                        }) $ model {
                              contacts = [contact],
                              chatting = Nothing
                        }
                  TUA.equal (Just {
                        status: Unread,
                        id: newMessageID,
                        content,
                        sender: anotherIMUserID,
                        recipient: recipientID,
                        date
                  }) $ getHistory contacts

            TU.test "receiveMessage adds contact if new" do
                  date <- liftEffect $ map DateTimeWrapper EN.nowDateTime
                  let   { contacts } = DT.fst <<< CIC.receiveMessage webSocket true (ClientMessage {
                              date,
                              id: newMessageID,
                              content,
                              userID: anotherIMUserID
                        }) $ model {
                              contacts = [],
                              chatting = Nothing
                        }
                  TUA.equal ((contacts !@ 0).history) [
                         {
                              status: Unread,
                              id: newMessageID,
                              sender: anotherIMUserID,
                              recipient: recipientID,
                              content,
                              date
                        }
                  ]

            TU.test "receiveMessage bumps chatting" do
                  date <- liftEffect $ map DateTimeWrapper EN.nowDateTime
                  let   { chatting } = DT.fst <<< CIC.receiveMessage webSocket true (ClientMessage {
                              date,
                              id: newMessageID,
                              content,
                              userID: anotherIMUserID
                        }) $ model {
                              contacts = [contact],
                              chatting = Just 0
                        }
                  TUA.equal (Just 1) chatting

            TU.test "receiveMessage set chatting if message comes from current suggestion" do
                  date <- liftEffect $ map DateTimeWrapper EN.nowDateTime
                  let   model' = model {
                              contacts = [],
                              chatting = Nothing,
                              suggesting = Just 0,
                              suggestions = [anotherIMUser]
                        }
                        { contacts, chatting } = DT.fst $ CIC.receiveMessage webSocket true (ClientMessage {
                              id: newMessageID,
                              userID: anotherIMUserID,
                              content,
                              date
                        }) model'
                        suggestionToContact = Just $ contact {
                              chatStarter = anotherIMUserID,
                              user = anotherIMUser,
                              history = [{
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
                  date <- liftEffect $ map DateTimeWrapper EN.nowDateTime
                  let   { contacts } = DT.fst <<< CIC.receiveMessage webSocket true (ClientMessage {
                              id: newMessageID,
                              userID: contactID,
                              content,
                              date
                        }) $ model {
                              contacts = [contact],
                              chatting = Just 0,
                              suggesting = Nothing
                        }
                  TUA.equal [Tuple newMessageID Read] $ map (\( { id, status}) -> Tuple id status) (contacts !@ 0).history

            TU.test "receiveMessage marks messages as read if window is not focused" do
                  date <- liftEffect $ map DateTimeWrapper EN.nowDateTime
                  let { contacts } = DT.fst <<< CIC.receiveMessage webSocket false (ClientMessage {
                              id: newMessageID,
                              userID: anotherIMUserID,
                              content,
                              date
                        }) $ model {
                              contacts = [contact],
                              chatting = Just 0,
                              suggesting = Nothing
                        }
                  TUA.equal [Tuple newMessageID Unread] $ map (\( { id, status}) -> Tuple id status) (contacts !@ 0).history

      where getHistory contacts = do
                  { history } <- DA.head contacts
                  DA.head history
            getMessageID contacts = do
                  { id } <- getHistory contacts
                  pure id