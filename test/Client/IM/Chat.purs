module Test.Client.IM.Chat where

import Prelude
import Shared.ContentType
import Shared.IM.Types

import Client.IM.Chat as CIC
import Data.Array ((!!), (:))
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.String as DS
import Data.Tuple as DT
import Effect.Class (liftEffect)
import Effect.Now as EN
import Shared.DateTime (DateTimeWrapper(..))
import Shared.IM.Contact as SIC
import Shared.Options.File (maxImageSize)
import Shared.Unsafe ((!@))
import Shared.Unsafe as SN
import Test.Client.Model (anotherImUser, anotherImUserId, contact, contactId, historyMessage, imUser, imUserId, model, suggestion, webSocket)
import Test.Unit (TestSuite)
import Test.Unit as TU
import Test.Unit.Assert as TUA

tests ∷ TestSuite
tests = do
      TU.suite "im chat update" do
            TU.test "beforeSendMessage adds new contact from suggestion" do
                  let
                        model' = model
                              { suggestions = suggestion : modelSuggestions
                              , chatting = Nothing
                              , suggesting = Just 0
                              }
                        { contacts } = DT.fst $ CIC.beforeSendMessage content model'
                  TUA.equal (_.user <$> DA.head contacts) $ Just suggestion

            TU.test "beforeSendMessage does not add new contact from suggestion if it already is on the list" do
                  let
                        model' = model
                              { suggestions = [contact.user]
                              , chatting = Nothing
                              , suggesting = Just 0
                              , contacts = [contact]
                              }
                        { contacts } = DT.fst $ CIC.beforeSendMessage content model'
                  TUA.equal (_.user <$> DA.head contacts) $ Just contact.user

            TU.test "beforeSendMessage sets chatting to existing contact index" do
                  let
                        model' = model
                              { suggestions = [contact.user]
                              , chatting = Nothing
                              , suggesting = Just 0
                              , contacts = [SIC.defaultContact 789 contact.user { id = 8}, contact]
                              }
                        { chatting } = DT.fst $ CIC.beforeSendMessage content model'
                  TUA.equal (Just 1) chatting

            TU.test "beforeSendMessage sets chatting to 0" do
                  let
                        model' = model
                              { suggestions = suggestion : modelSuggestions
                              , chatting = Nothing
                              , suggesting = Just 0
                              }
                        { chatting } = DT.fst $ CIC.beforeSendMessage content model'
                  TUA.equal (Just 0) chatting

            TU.test "sendMessage bumps temporary id" do
                  date ← liftEffect $ map DateTimeWrapper EN.nowDateTime
                  let m@{ temporaryId } = DT.fst $ CIC.sendMessage webSocket content date model
                  TUA.equal 1 temporaryId

                  let { temporaryId } = DT.fst $ CIC.sendMessage webSocket content date m
                  TUA.equal 2 temporaryId

            TU.test "sendMessage adds message to history" do
                  date ← liftEffect $ map DateTimeWrapper EN.nowDateTime
                  let
                        { user: { id: userId }, contacts, chatting } = DT.fst $ CIC.sendMessage webSocket content date model
                        user = SN.fromJust do
                              index ← chatting
                              contacts !! index

                  TUA.equal
                        [ { date: (user.history !@ 0).date
                          , recipient: user.user.id
                          , status: Sent
                          , id: 1
                          , content: "test"
                          , sender: userId
                          }
                        ]
                        user.history

            TU.test "sendMessage adds markdown image to history" do
                  date ← liftEffect $ map DateTimeWrapper EN.nowDateTime
                  let
                        { contacts, chatting } = DT.fst <<< CIC.sendMessage webSocket (Image caption image) date $ model
                              { selectedImage = Just image
                              , imageCaption = Just caption
                              }
                        entry = SN.fromJust do
                              index ← chatting
                              contacts !! index

                  TUA.equal ("![" <> caption <> "](" <> image <> ")") (entry.history !@ 0).content

            TU.test "sendMessage resets input fields" do
                  date ← liftEffect $ map DateTimeWrapper EN.nowDateTime
                  let
                        { selectedImage, imageCaption } = DT.fst <<< CIC.sendMessage webSocket content date $ model
                              { selectedImage = Just image
                              , imageCaption = Just caption
                              }
                  TUA.equal Nothing selectedImage
                  TUA.equal Nothing imageCaption

            TU.test "makeTurn calculate turn" do
                  let
                        contact' = contact
                              { history = [ recipientMessage, senderMessage ]
                              }
                        turn = Just
                              { chatAge: 0.0
                              , recipientStats:
                                      { characters: 4.0
                                      , interest: 1.0
                                      }
                              , replyDelay: 0.0
                              , senderStats:
                                      { characters: 4.0
                                      , interest: 1.0
                                      }
                              }
                  TUA.equal turn $ CIC.makeTurn contact' imUserId

            TU.test "makeTurn don't calculate turn for recipient" do
                  TUA.equal Nothing $ CIC.makeTurn contact 90000

            TU.test "makeTurn don't calculate turn if last message isn't from the sender" do
                  let
                        contact' = contact
                              { history = [ recipientMessage, recipientMessage ]
                              }

                  TUA.equal Nothing $ CIC.makeTurn contact' contactId

            TU.test "setSelectedImage sets file" do
                  let
                        { selectedImage, erroredFields } = DT.fst <<< CIC.setSelectedImage (Just image) $ model
                              { erroredFields = []
                              , selectedImage = Nothing
                              }
                  TUA.equal (Just image) selectedImage
                  TUA.equal [] erroredFields

            TU.test "setSelectedImage validates files too long" do
                  let
                        { erroredFields } = DT.fst <<< CIC.setSelectedImage (Just <<< DS.joinWith "" $ DA.replicate (maxImageSize * 10) "a") $ model
                              { erroredFields = []
                              }
                  TUA.equal [ "selectedImage" ] erroredFields

      where
      getHistory contacts = do
            { history } ← DA.head contacts
            DA.head history
      getMessageID contacts = do
            { id } ← getHistory contacts
            pure id

      content = Text "test"
      { id: recipientId } = imUser
      messageId = 1
      newMessageID = 101
      { suggestions: modelSuggestions } = model

      recipientMessage = historyMessage
            { sender = anotherImUserId
            , recipient = imUserId
            }
      senderMessage = historyMessage
            { sender = imUserId
            , recipient = anotherImUserId
            }
      image = "base64"
      caption = "caption"