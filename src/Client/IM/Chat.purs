-- | This module takes care of websocket plus chat editor events.
module Client.IM.Chat where

import Debug.Trace
import Prelude
import Shared.IM.Types
import Shared.Types

import Client.Common.DOM as CCD
import Client.Common.File as CCF
import Client.Common.Network as CCNT
import Client.IM.Contacts as CICN
import Client.IM.Flame (NextMessage, NoMessages, MoreMessages)
import Client.IM.Flame as CIF
import Client.IM.Scroll as CIS
import Client.IM.WebSocket as CIW
import Data.Array ((:), (!!))
import Data.Array as DA
import Data.DateTime as DT
import Data.Either (Either(..))
import Data.Either as DET
import Data.Int as DI
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Newtype (class Newtype)
import Data.Newtype as DN
import Data.String as DS
import Data.String.CodeUnits as DSC
import Data.Time.Duration (Seconds(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console as EC
import Effect.Now as EN
import Flame (ListUpdate, (:>))
import Flame as F
import Shared.IM.Contact as SIC
import Shared.Newtype as SN
import Shared.PrimaryKey as SP
import Shared.Unsafe ((!@))
import Shared.Unsafe as SU
import Signal.Channel (Channel)
import Web.DOM (Element)
import Web.Event.Event (Event)
import Web.Event.Event as WEE
import Web.File.FileList as WFF
import Web.File.FileReader (FileReader, fileReader)
import Web.HTML.Event.DataTransfer as WHEDT
import Web.HTML.Event.DragEvent (DragEvent)
import Web.HTML.Event.DragEvent as WHED
import Web.HTML.HTMLElement as WHHEL
import Web.HTML.HTMLTextAreaElement as WHHTA
import Web.Socket.WebSocket (WebSocket)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as WUK

--purty is fucking terrible

getFileInput :: Effect Element
--REFACTOR: make selectors inside updates type safe
getFileInput = CCD.querySelector "#image-file-input"

setUpMessage :: IMModel -> Event -> NextMessage
setUpMessage model@(IMModel { messageEnter }) event = model :> [liftEffect do
      let   keyboardEvent = SU.fromJust $ WUK.fromEvent event
            textarea = SU.fromJust  do
                  target <- WEE.target event
                  WHHTA.fromEventTarget target
      content <- WHHTA.value textarea
      let sent = messageEnter && WUK.key keyboardEvent == "Enter" && not WUK.shiftKey keyboardEvent
      when sent $ CCD.preventStop event
      CIF.next $ BeforeSendMessage sent content
]

beforeSendMessage :: IMModel -> Boolean -> String -> MoreMessages
beforeSendMessage model@(IMModel {
      chatting,
      user: IMUser { id },
      contacts,
      suggesting,
      suggestions
}) sent content
      | sent = snocContact :> [ nextSendMessage ]

      where snocContact = case Tuple chatting suggesting of
                  Tuple Nothing (Just index) ->
                        let chatted = suggestions !@ index
                        in
                              SN.updateModel model $ _ {
                                    message = Just content,
                                    chatting = Just 0,
                                    suggesting = Nothing,
                                    contacts = DA.cons (SIC.defaultContact id chatted) contacts,
                                    suggestions = SU.fromJust  $ DA.deleteAt index suggestions
                              }
                  _ -> model

            nextSendMessage = do
                  date <- liftEffect $ map MDateTime EN.nowDateTime
                  CIF.next $ SendMessage date

      | otherwise =
            F.noMessages <<< SN.updateModel model $ _ {
                  message = Just content
            }

sendMessage :: WebSocket -> String -> MDateTime -> IMModel -> NoMessages
sendMessage webSocket token date = case _ of
      model@(IMModel {
            user: IMUser { id: senderID },
            chatting: Just chatting,
            temporaryID,
            contacts,
            message,
            selectedImage,
            imageCaption
      }) ->
            let  recipient@(Contact { user: IMUser { id: recipientID }, history }) = contacts !@ chatting
                 newTemporaryID = temporaryID + SP.fromInt 1
                 updatedChatting = SN.updateContact recipient $ _ {
                        history = DA.snoc history $ HistoryMessage {
                              id: newTemporaryID,
                              status: Unread,
                              sender: senderID,
                              recipient: recipientID,
                              date,
                              content: DM.maybe' (\_ -> SU.fromJust message) (asMarkdownImage imageCaption) selectedImage
                        }
                 }
                 updatedModel = SN.updateModel model $ _ {
                        temporaryID = newTemporaryID,
                        imageCaption = Nothing,
                        selectedImage = Nothing,
                        message = if DM.isJust selectedImage then message else Nothing,
                        contacts = SU.fromJust $ DA.updateAt chatting updatedChatting contacts
                 }
                 turn = makeTurn updatedChatting senderID
            in --needs to handle failure!
                  CIF.nothingNext updatedModel $ liftEffect do
                        CIS.scrollLastMessage
                        CIW.sendPayload webSocket $ ServerMessage {
                              id: newTemporaryID,
                              userID: recipientID,
                              token: token,
                              content: asMessageContent message imageCaption selectedImage,
                              turn
                        }
      model -> F.noMessages model
      where asMarkdownImage imageCaption base64 = "![" <> DM.fromMaybe "" imageCaption  <> "](" <> base64 <> ")"
            asMessageContent message imageCaption selectedImage = DM.maybe' (\_ -> Text $ SU.fromJust message) (Image <<< Tuple (DM.fromMaybe "" imageCaption)) selectedImage

makeTurn :: Contact -> PrimaryKey -> Maybe Turn
makeTurn (Contact { chatStarter, chatAge, history }) sender =
      if chatStarter == sender && isNewTurn history sender then
            let   senderEntry = SU.fromJust $ DA.last history
                  recipientEntry = SU.fromJust $ history !! (DA.length history - 2)
                  Tuple senderMessages recipientMessages = SU.fromJust do
                        let   groups = DA.groupBy sameSender history
                              size = DA.length groups
                        senderMessages <- groups !! (size - 3)
                        recipientMessages <- groups !! (size - 2)
                        pure $ Tuple senderMessages recipientMessages
                  senderCharacters = DI.toNumber $ DA.foldl countCharacters 0 senderMessages
                  recipientCharacters = DI.toNumber $ DA.foldl countCharacters 0 recipientMessages
            in
                  Just $ Turn {
                        senderStats: Stats {
                              characters: senderCharacters,
                              interest: senderCharacters / recipientCharacters
                        },
                        recipientStats: Stats {
                              characters: recipientCharacters,
                              interest: recipientCharacters / senderCharacters
                        },
                        replyDelay: DN.unwrap (DT.diff (getDate senderEntry) $ getDate recipientEntry :: Seconds),
                        chatAge
                  }
       else
            Nothing
      where isNewTurn history userID = DM.fromMaybe false do
                  last <- DA.last history
                  beforeLast <- history !! (DA.length history - 2)
                  let sender = _.sender $ DN.unwrap last
                      recipient = _.recipient $ DN.unwrap beforeLast
                  pure $ sender == userID && recipient == userID

            sameSender entry anotherEntry = _.sender (DN.unwrap entry) == _.sender (DN.unwrap anotherEntry)
            countCharacters total (HistoryMessage { content }) = total + DSC.length content
            getDate = DN.unwrap <<< _.date <<< DN.unwrap

receiveMessage :: WebSocket -> String -> Boolean -> IMModel -> WebSocketPayloadClient -> MoreMessages
receiveMessage webSocket token isFocused model@(IMModel {
      user: IMUser { id: recipientID },
      contacts,
      suggestions
}) = case _ of
      Received { previousID, id } ->
            F.noMessages <<< SN.updateModel model $ _ {
                  contacts = DM.fromMaybe contacts $ updateTemporaryID contacts previousID id
            }
      ClientMessage payload@{ userID } -> case processIncomingMessage payload model of
            Left userID -> model :> [Just <<< DisplayContacts <$> CCNT.get' (SingleContact { id: userID })]
            Right updatedModel@(IMModel {
                  chatting: Just index,
                  contacts
            }) ->  --mark it as read if we received a message from the current chat
                  let fields = {
                        chatting: index,
                        userID: recipientID,
                        contacts,
                        token,
                        webSocket
                  }
                  in
                        if isFocused && isChatting userID fields then
                              CICN.updateReadHistory updatedModel fields
                         else
                              F.noMessages updatedModel
            Right updatedModel -> F.noMessages updatedModel
      where isChatting senderID { contacts, chatting } =
                  let (Contact { user: IMUser { id: recipientID } }) = contacts !@ chatting in
                  recipientID == senderID

processIncomingMessage :: ClientMessagePayload -> IMModel -> Either PrimaryKey IMModel
processIncomingMessage { id, userID, date, content } model@(IMModel {
      user: IMUser { id: recipientID },
      suggestions,
      contacts,
      suggesting,
      chatting
}) = case findAndUpdateContactList of
      Just contacts' ->
            --new messages bubble the contact to the top
            let added = DA.head contacts' in Right $
                  if getUserID (map (_.user <<< DN.unwrap) added) == getUserID suggestingContact then
                        --edge case of receiving a message from a suggestion
                        SN.updateModel model $ _ {
                              contacts = contacts',
                              suggesting = Nothing,
                              suggestions = SU.fromJust do
                                    index <- suggesting
                                    DA.deleteAt index suggestions,
                              chatting = Just 0
                        }
                   else
                        SN.updateModel model $ _ {
                              contacts = contacts',
                              --since the contact list is altered, the chatting index must be bumped
                              chatting = (_ + 1) <$> chatting
                        }
      Nothing -> Left userID
      where suggestingContact = do
                  index <- suggesting
                  suggestions !! index

            getUserID :: forall n a. Newtype n { id :: PrimaryKey | a } => Maybe n -> Maybe PrimaryKey
            getUserID = map (_.id <<< DN.unwrap)
            findUser (Contact { user: IMUser { id } }) = userID == id

            updateHistory { id, content, date } user@(Contact { history }) =
                  SN.updateContact user $ _ {
                        history = DA.snoc history $ HistoryMessage {
                              status: Unread,
                              sender: userID,
                              recipient: recipientID,
                              id,
                              content,
                              date
                        }
                  }
            findAndUpdateContactList = do
                  index <- DA.findIndex findUser contacts
                  Contact { history } <- contacts !! index
                  DA.modifyAt index (updateHistory { content, id, date }) contacts

updateTemporaryID :: Array Contact -> PrimaryKey -> PrimaryKey -> Maybe (Array Contact)
updateTemporaryID contacts previousID id = do
      index <- DA.findIndex (findUser previousID) contacts
      Contact { history } <- contacts !! index
      innerIndex <- DA.findIndex (findTemporary previousID) history
      DA.modifyAt index (updateTemporary innerIndex id) contacts

      where findTemporary previousID (HistoryMessage { id }) = id == previousID
            findUser previousID (Contact { history }) = DA.any (findTemporary previousID) history
            updateTemporary index newID user@(Contact { history }) =
                  SN.updateContact user $ _ {
                        history = SU.fromJust $ DA.modifyAt index (flip SN.updateHistoryMessage (_ { id = newID })) history
                  }

applyMarkup :: Markup -> IMModel -> MoreMessages
applyMarkup markup model@(IMModel { message }) = model :> [liftEffect (Just <$> apply markup (DM.fromMaybe "" message))]

apply :: Markup -> String -> Effect IMMessage
apply markup value = do
      textarea <- SU.fromJust <<< WHHTA.fromElement <$> CCD.querySelector "#chat-input"
      let   Tuple before after = case markup of
                  Bold -> Tuple "**" "**"
                  Italic -> Tuple "*" "*"
                  Strike -> Tuple "~" "~"
                  Heading -> Tuple "## " ""
                  OrderedList -> Tuple "1. " ""
                  UnorderedList -> Tuple "- " ""
      start <- WHHTA.selectionStart textarea
      end <- WHHTA.selectionEnd textarea
      let   beforeSize = DS.length before
            beforeSelection = DS.take start value
            selected = DS.take (end - start) $ DS.drop start value
            afterSelection = DS.drop end value
            newValue = beforeSelection <> before <> selected <> after <> afterSelection
      pure $ SetMessageContent (Just $ end + beforeSize) newValue

--REFACTOR: this can be abstracted (together with profile updates?)
preview ::  IMModel -> NoMessages
preview model =
      F.noMessages <<< SN.updateModel model $ _ {
            isPreviewing = true
      }

exitPreview :: IMModel -> NextMessage
exitPreview model =
      F.noMessages <<< SN.updateModel model $ _ {
            isPreviewing = false
      }

setMessage :: Maybe Int -> String -> IMModel -> NextMessage
setMessage cursor markdown model =
      CIF.nothingNext (SN.updateModel model $ _ {
            message = Just markdown
      }) <<< liftEffect $
            case cursor of
                  Just position -> do
                        textarea <- SU.fromJust <<< WHHTA.fromElement <$> CCD.querySelector "#chat-input"
                        WHHEL.focus $ WHHTA.toHTMLElement textarea
                        WHHTA.setSelectionEnd position textarea
                  Nothing -> pure unit

selectImage :: IMModel -> NextMessage
selectImage model = CIF.nothingNext model $ liftEffect do
      input <- getFileInput
      CCF.triggerFileSelect input

toggleImageForm :: IMModel -> Maybe String -> NoMessages
toggleImageForm model base64 =
      F.noMessages <<< SN.updateModel model $ _ {
            selectedImage = base64
      }

setImageCaption :: String -> IMModel -> NoMessages
setImageCaption caption model =
      F.noMessages <<< SN.updateModel model $ _ {
            imageCaption = Just caption
      }

catchFile :: IMModel -> FileReader -> Event -> NoMessages
catchFile model fileReader event = CIF.nothingNext model $ liftEffect do
      CCF.readBase64 fileReader <<< WHEDT.files <<< WHED.dataTransfer <<< SU.fromJust $ WHED.fromEvent event
      CCD.preventStop event

toggleMessageEnter :: IMModel -> NoMessages
toggleMessageEnter model@(IMModel { messageEnter }) =
      F.noMessages <<< SN.updateModel model $ _ {
            messageEnter = not messageEnter
      }

toggleEmojisVisible :: IMModel -> NoMessages
toggleEmojisVisible model@(IMModel { emojisVisible }) =
      F.noMessages <<< SN.updateModel model $ _ {
            emojisVisible = not emojisVisible
      }

setEmoji :: IMModel -> Event -> NextMessage
setEmoji model@(IMModel { message }) event = SN.updateModel model (_ {
      emojisVisible = false
}) :> [liftEffect do
      emoji <- CCD.innerTextFromTarget event
      textarea <- SU.fromJust <<< WHHTA.fromElement <$> CCD.querySelector "#chat-input"
      end <- WHHTA.selectionEnd textarea
      let { before, after } = DS.splitAt end $ DM.fromMaybe "" message
      CIF.next <<< SetMessageContent (Just $ end + 2) $ before <> emoji <> after
]