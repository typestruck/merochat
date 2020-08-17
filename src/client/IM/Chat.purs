-- | This module takes care of websocket plus chat editor events.
module Client.IM.Chat where

import Prelude
import Shared.IM.Types
import Shared.Types

import Client.Common.DOM as CCD
import Client.Common.File as CCF
import Client.Common.Network as CCNT
import Client.Common.Notification as CCN
import Client.IM.Contacts as CICN
import Client.IM.Flame (NextMessage, NoMessages, MoreMessages)
import Client.IM.Flame as CIF
import Client.IM.Scroll as CIS
import Client.IM.Suggestion as CISG
import Client.IM.WebSocket as CIW
import Data.Array ((!!))
import Data.Array as DA
import Data.DateTime as DT
import Data.Either (Either(..))
import Data.Int as DI
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Newtype (class Newtype)
import Data.Newtype as DN
import Data.Nullable (null)
import Data.String as DS
import Data.String.CodeUnits as DSC
import Data.Time.Duration (Seconds)
import Data.Tuple (Tuple(..))
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Now as EN
import Flame ((:>))
import Flame as F
import Node.URL as NU
import Shared.IM.Contact as SIC
import Shared.Newtype as SN
import Shared.PrimaryKey as SP
import Shared.Unsafe ((!@))
import Shared.Unsafe as SU
import Web.DOM (Element)
import Web.Event.Event (Event)
import Web.Event.Event as WEE
import Web.File.FileReader (FileReader)
import Web.HTML.Event.DataTransfer as WHEDT
import Web.HTML.Event.DragEvent as WHED
import Web.HTML.HTMLElement as WHHEL
import Web.HTML.HTMLTextAreaElement as WHHTA
import Web.Socket.WebSocket (WebSocket)
import Web.UIEvent.KeyboardEvent as WUK

--purty is fucking terrible

--REFACTOR: make selectors inside updates type safe
getFileInput :: Effect Element
getFileInput = CCD.querySelector "#image-file-input"

setUpMessage :: Event -> IMModel -> NextMessage
setUpMessage event model@(IMModel { emojisVisible, messageEnter }) = model :> [beforeSend]
      where beforeSend = liftEffect do
                  let   textarea = SU.fromJust  do
                              target <- WEE.target event
                              WHHTA.fromEventTarget target
                        sent = messageEnter && key == "Enter" && not WUK.shiftKey keyboardEvent
                  content <- WHHTA.value textarea
                  when sent $ CCD.preventStop event
                  CIF.next $ BeforeSendMessage sent content

            keyboardEvent = SU.fromJust $ WUK.fromEvent event
            key = WUK.key keyboardEvent

beforeSendMessage :: Boolean -> String -> IMModel -> MoreMessages
beforeSendMessage sent content model@(IMModel {
      chatting,
      user: IMUser { id },
      contacts,
      suggesting,
      suggestions
})    | sent = snocContact :> [ nextSendMessage ]

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

sendMessage :: WebSocket -> MDateTime -> IMModel -> NoMessages
sendMessage webSocket date = case _ of
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
            in
                  CIF.nothingNext updatedModel $ liftEffect do
                        CIS.scrollLastMessage
                        CIW.sendPayload webSocket $ ServerMessage {
                              id: newTemporaryID,
                              userID: recipientID,
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

receiveMessage :: WebSocket -> Boolean -> WebSocketPayloadClient -> IMModel -> MoreMessages
receiveMessage webSocket isFocused wsPayload model@(IMModel {
      user: IMUser { id: recipientID },
      contacts,
      suggestions,
      blockedUsers
}) = case wsPayload of
      Received { previousID, id, userID } ->
            F.noMessages <<< SN.updateModel model $ _ {
                  contacts = updateTemporaryID contacts userID previousID id
            }
      BeenBlocked { id } ->
            F.noMessages $ CISG.removeBlockedUser id model
      ClientMessage payload@{ userID } ->
            if DA.elem userID blockedUsers then
                  F.noMessages model
            else case processIncomingMessage payload model of
                  Left userID -> model :> [Just <<< DisplayContacts <$> CCNT.get' (SingleContact { id: userID })]
                  Right updatedModel@(IMModel {
                        chatting: Just index,
                        contacts
                  }) ->  --mark it as read if we received a message from the current chat
                        let fields = {
                              chatting: index,
                              userID: recipientID,
                              contacts,
                              webSocket
                        }
                        in
                              if isFocused && isChatting userID fields then
                                    CICN.updateReadHistory updatedModel fields
                              else
                                    F.noMessages updatedModel
                  Right updatedModel -> F.noMessages updatedModel
      PayloadError payload -> case payload of
            ServerMessage { id, userID } -> F.noMessages <<< SN.updateModel model $ _ {
                 contacts = updateHistoryStatus contacts userID id
            }
            --the connection might still be open and the server haven't saved the socket
            Connect -> CIF.nothingNext (spy "s" model) <<< liftEffect $ CIW.close webSocket
            _ -> F.noMessages model
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
      where updateHistory { id, content, date } user@(Contact { history }) =
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

            suggestingContact = do
                  index <- suggesting
                  suggestions !! index

            getUserID :: forall n a. Newtype n { id :: PrimaryKey | a } => Maybe n -> Maybe PrimaryKey
            getUserID = map (_.id <<< DN.unwrap)
            findUser (Contact { user: IMUser { id } }) = userID == id

updateTemporaryID :: Array Contact -> PrimaryKey -> PrimaryKey -> PrimaryKey -> Array Contact
updateTemporaryID contacts userID previousMessageID messageID = updateContactHistory contacts userID updateTemporary
      where updateTemporary history@(HistoryMessage { id })
                  | id == previousMessageID = SN.updateHistoryMessage history $ _ { id = messageID }
                  | otherwise = history

updateHistoryStatus :: Array Contact -> PrimaryKey -> PrimaryKey -> Array Contact
updateHistoryStatus contacts userID messageID = updateContactHistory contacts userID updateStatus
      where updateStatus history@(HistoryMessage { id })
                  | id == messageID = SN.updateHistoryMessage history $ _ { status = Errored }
                  | otherwise = history

updateContactHistory :: Array Contact -> PrimaryKey -> (HistoryMessage -> HistoryMessage) -> Array Contact
updateContactHistory contacts userID f = updateContact <$> contacts
      where updateContact contact@(Contact { user: IMUser { id }, history })
                  | id == userID = SN.updateContact contact $ _ {
                              history = f <$> history
                        }
                  | otherwise = contact

applyMarkup :: Markup -> IMModel -> MoreMessages
applyMarkup markup model@(IMModel { message }) = model :> [liftEffect (Just <$> apply markup (DM.fromMaybe "" message))]
      where apply markup value = do
                  textarea <- SU.fromJust <<< WHHTA.fromElement <$> CCD.querySelector "#chat-input"
                  let   Tuple before after = case markup of
                              Bold -> Tuple "**" "**"
                              Italic -> Tuple "*" "*"
                              Strike -> Tuple "~" "~"
                              Heading -> Tuple "## " ""
                              OrderedList -> Tuple "\n1. " ""
                              UnorderedList -> Tuple "\n- " ""
                  start <- WHHTA.selectionStart textarea
                  end <- WHHTA.selectionEnd textarea
                  let   beforeSize = DS.length before
                        beforeSelection = DS.take start value
                        selected = DS.take (end - start) $ DS.drop start value
                        afterSelection = DS.drop end value
                        newValue = beforeSelection <> before <> selected <> after <> afterSelection
                  pure $ SetMessageContent (Just $ end + beforeSize) newValue

preview :: IMModel -> NoMessages
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

toggleImageForm :: Maybe String -> IMModel -> NoMessages
toggleImageForm base64 model =
      F.noMessages <<< SN.updateModel model $ _ {
            selectedImage = base64
      }

setImageCaption :: String -> IMModel -> NoMessages
setImageCaption caption model =
      F.noMessages <<< SN.updateModel model $ _ {
            imageCaption = Just caption
      }

catchFile :: FileReader -> Event -> IMModel -> NoMessages
catchFile fileReader event model = CIF.nothingNext model $ liftEffect do
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

toggleLinkForm :: IMModel -> NoMessages
toggleLinkForm model@(IMModel { linkFormVisible }) =
      F.noMessages <<< SN.updateModel model $ _ {
            linkFormVisible = not linkFormVisible,
            link = Nothing,
            linkText = Nothing
      }

setEmoji :: Event -> IMModel -> NextMessage
setEmoji event model@(IMModel { message }) = SN.updateModel model (_ {
      emojisVisible = false
}) :> [liftEffect do
      emoji <- CCD.innerTextFromTarget event
      setAtCursor message emoji
]

setLinkText :: String -> IMModel -> NoMessages
setLinkText text model =
      F.noMessages <<< SN.updateModel model $ _ {
            linkText = Just text
      }

setLink :: String -> IMModel -> NoMessages
setLink link model =
      F.noMessages <<< SN.updateModel model $ _ {
            link = Just link
      }

insertLink :: IMModel -> NextMessage
insertLink model@(IMModel { message, linkText, link }) =
      case link of
            Nothing -> CIF.nothingNext model <<< liftEffect $ CCN.alert "Link is required"
            Just url ->
                  let { protocol } = NU.parse $ DS.trim url
                  in model :> [
                        CIF.next ToggleLinkForm,
                        insert $ if protocol == null then "http://" <> url else url
                  ]
      where markdown url = "[" <> DM.fromMaybe url linkText <> "](" <> url <> ")"
            insert = liftEffect <<< setAtCursor message <<< markdown

setAtCursor :: Maybe String -> String -> Effect (Maybe IMMessage)
setAtCursor message text = do
      textarea <- SU.fromJust <<< WHHTA.fromElement <$> CCD.querySelector "#chat-input"
      end <- WHHTA.selectionEnd textarea
      let { before, after } = DS.splitAt end $ DM.fromMaybe "" message
      CIF.next <<< SetMessageContent (Just $ end + DS.length text + 1) $ before <> text <> after