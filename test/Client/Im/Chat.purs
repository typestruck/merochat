module Test.Client.Im.Chat where

import Prelude

import Client.Im.Chat as CIC
import Data.Array ((!!), (:))
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.String as DS
import Data.Tuple as DT
import Effect.Class (liftEffect)
import Effect.Now as EN
import Shared.DateTime (DateTimeWrapper(..))
import Shared.Element (ElementId(..))
import Shared.Im.Contact as SIC
import Shared.Im.Types (MessageContent(..), MessageStatus(..))
import Shared.Resource (maxImageSize)
import Shared.Unsafe ((!@))
import Shared.Unsafe as SN
import Shared.Unsafe as SU
import Test.Client.Model (anotherImUserId, contact, historyMessage, imUser, imUserId, model, suggestion, webSocket)
import Test.Unit (TestSuite)
import Test.Unit as TU
import Test.Unit.Assert as TUA

tests ∷ TestSuite
tests = do
      TU.suite "im chat update" do
            TU.test "prepareSendMessage adds new contact from suggestion" do
                  date ← liftEffect $ map DateTimeWrapper EN.nowDateTime
                  let
                        model' = model
                              { suggestions = suggestion : modelSuggestions
                              , chatting = Nothing
                              , suggesting = Just  0
                              }
                        { contacts } = DT.fst $ CIC.prepareSendMessage ChatInput content date webSocket model'
                  TUA.equal (_.user <$> DA.head contacts) $ Just suggestion

            TU.test "prepareSendMessage does not add new contact from suggestion if it already is on the list" do
                  date ← liftEffect $ map DateTimeWrapper EN.nowDateTime
                  let
                        model' = model
                              { suggestions = [ contact.user ]
                              , chatting = Nothing
                              , suggesting = Just 0
                              , contacts = [ contact ]
                              }
                        { contacts } = DT.fst $ CIC.prepareSendMessage ChatInput content date webSocket model'
                  TUA.equal (_.user <$> DA.head contacts) $ Just contact.user

            TU.test "sendMessage bumps temporary id" do
                  date ← liftEffect $ map DateTimeWrapper EN.nowDateTime
                  let m@{ temporaryId } = DT.fst $ CIC.sendMessage 3 true content date webSocket model
                  TUA.equal 1 temporaryId

                  let { temporaryId } = DT.fst $ CIC.sendMessage 3 true content date webSocket m
                  TUA.equal 2 temporaryId

            TU.test "sendMessage adds message to history" do
                  date ← liftEffect $ map DateTimeWrapper EN.nowDateTime
                  let
                        { user: { id: userId }, chatting } = DT.fst $ CIC.sendMessage 3 true content date webSocket model
                        user = SU.fromJust $ SIC.maybeFindContact chatting model.contacts

                  TUA.equal
                        [ { date: (user.history !@ 0).date
                          , recipient: user.user.id
                          , status: Sent
                          , id: 1
                          , edited: false
                          , content: "test"
                          , sender: userId
                          }
                        ]
                        user.history

            TU.test "sendMessage adds markdown image to history" do
                  date ← liftEffect $ map DateTimeWrapper EN.nowDateTime
                  let
                        { chatting } = DT.fst <<< CIC.sendMessage (SU.fromJust model.chatting) true (Image caption 2 2 image) date webSocket $ model
                              { selectedImage = Just { base64: image, width : 2, height : 2 }
                              , imageCaption = Just caption
                              }
                        entry = SU.fromJust $ SIC.maybeFindContact chatting model.contacts

                  TUA.equal ("![" <> caption <> "](" <> image <> ")") (entry.history !@ 0).content

            TU.test "sendMessage resets input fields" do
                  date ← liftEffect $ map DateTimeWrapper EN.nowDateTime
                  let
                        { selectedImage, imageCaption } = DT.fst <<< CIC.sendMessage (SU.fromJust model.chatting) true content date webSocket $ model
                              { selectedImage = Just { base64: image, width : 2, height : 2 }
                              , imageCaption = Just caption
                              }
                  TUA.equal Nothing selectedImage
                  TUA.equal Nothing imageCaption

            TU.test "makeTurn calculates recipient characters" do
                  let
                        contact' = contact
                              { history = [ senderMessage, recipientMessage { content = "hello" }, senderMessage ]
                              }
                  TUA.equal (Just 5.0) (_.characters <<< _.recipientStats <$> CIC.makeTurn imUser contact')

            TU.test "makeTurn calculates sender characters" do
                  let
                        contact' = contact
                              { history = [ senderMessage { content = "hello" }, recipientMessage, senderMessage { content = "hello" }, senderMessage { content = "hello" }, senderMessage { content = "hello" }, recipientMessage, senderMessage { content = "hello" } ]
                              }
                  TUA.equal (Just 15.0) (_.characters <<< _.senderStats <$> CIC.makeTurn imUser contact')

            TU.test "makeTurn calculates recipient interest" do
                  let
                        contact' = contact
                              { history = [ senderMessage { content = "hello" }, recipientMessage { content = "hello" }, senderMessage ]
                              }

                  TUA.equal (Just $ Just 1.0) (_.interest <<< _.recipientStats <$> CIC.makeTurn imUser contact')

            TU.test "makeTurn calculates sender interest for first message" do
                  let
                        contact' = contact
                              { history = [ senderMessage { content = "hello" }, recipientMessage, senderMessage { content = "hello" } ]
                              }
                  TUA.equal (Just Nothing) (_.interest <<< _.senderStats <$> CIC.makeTurn imUser contact')

            TU.test "makeTurn calculates sender interest for later message" do
                  let
                        contact' = contact
                              { history = [ recipientMessage { content = "hello" }, senderMessage { content = "hello" }, recipientMessage, senderMessage { content = "oi" } ]
                              }
                  TUA.equal (Just $ Just 1.0) (_.interest <<< _.senderStats <$> CIC.makeTurn imUser contact')

            -- TU.testOnly "makeTurn calculates recipient reply delay" do
            -- TU.testOnly "makeTurn calculates sender reply delay" do

            TU.test "makeTurn don't calculate turn for recipient" do
                  let
                        contact' = contact
                              { chatStarter = contact.user.id
                              , history = [ senderMessage, recipientMessage, senderMessage ]
                              }
                  TUA.equal Nothing $ CIC.makeTurn imUser contact'

            TU.test "makeTurn don't calculate turn if last message isn't from chat starter" do
                  let
                        contact' = contact
                              { history = [ recipientMessage, recipientMessage ]
                              }

                  TUA.equal Nothing $ CIC.makeTurn contact.user contact'

            TU.test "setSelectedImage sets file" do
                  let
                        { selectedImage, erroredFields } = DT.fst <<< CIC.setSelectedImage (Just { base64: image, width : 2, height : 2 }) $ model
                              { erroredFields = []
                              , selectedImage = Nothing
                              }
                  TUA.equal (Just { base64: image, width : 2, height : 2 }) selectedImage
                  TUA.equal [] erroredFields

            TU.test "setSelectedImage validates files too long" do
                  let
                        { erroredFields } = DT.fst <<< CIC.setSelectedImage (Just { base64:  DS.joinWith "" $ DA.replicate (maxImageSize * 10) "a", width : 2, height : 2 }) $ model
                              { erroredFields = []
                              }
                  TUA.equal [ "selectedImage" ] erroredFields

      where
      getHistory contacts = do
            { history } ← DA.head contacts
            DA.head history

      content = Text "test"
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