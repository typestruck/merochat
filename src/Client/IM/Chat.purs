module Client.IM.Chat where

import Prelude
import Shared.Types

import Client.Common.DOM as CCD
import Client.Common.File as CCF
import Client.Common.Notification as CCN
import Client.IM.Flame (NextMessage, NoMessages, MoreMessages)
import Client.IM.Flame as CIF
import Client.IM.Scroll as CIS
import Client.IM.WebSocket as CIW
import Data.Array ((!!), (:))
import Data.Array as DA
import Data.DateTime as DT
import Data.Int as DI
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Newtype as DN
import Data.Nullable (null)
import Data.String as DS
import Data.String.CodeUnits as DSC
import Data.Symbol (SProxy(..))
import Data.Symbol as TDS
import Data.Time.Duration (Seconds)
import Data.Tuple (Tuple(..))
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Now as EN
import Flame ((:>))
import Flame as F
import Node.URL as NU
import Shared.Options.File (maxImageSize)
import Shared.IM.Contact as SIC
import Shared.Unsafe ((!@))
import Shared.Unsafe as SU
import Web.DOM (Element)
import Web.Event.Event (Event)
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
getFileInput = CCD.unsafeQuerySelector "#image-file-input"

--the keydown event fires before input
enterBeforeSendMessage :: IMModel -> NoMessages
enterBeforeSendMessage model@{ messageEnter } = F.noMessages $ model {
      shouldSendMessage = messageEnter
}

--send message or image button click
forceBeforeSendMessage :: IMModel -> MoreMessages
forceBeforeSendMessage model@{ message }
      | DM.maybe true (DS.null <<< DS.trim) message = F.noMessages model
      | otherwise = model {
            shouldSendMessage = true
      } :> [
            pure <<< Just <<< BeforeSendMessage $ SU.fromJust message
      ]

--input event, or called after clicking the send message/image button
beforeSendMessage :: String -> IMModel -> MoreMessages
beforeSendMessage content model@{
      shouldSendMessage,
      chatting,
      user: { id },
      contacts,
      suggesting,
      suggestions
}    | shouldSendMessage = snocContact :> [ nextSendMessage ]

      where snocContact = case Tuple chatting suggesting of
                  Tuple Nothing (Just index) ->
                        let chatted = suggestions !@ index
                        in
                              model {
                                    message = Just content,
                                    chatting = Just 0,
                                    suggesting = Nothing,
                                    contacts = DA.cons (SIC.defaultContact id chatted) contacts,
                                    suggestions = SU.fromJust $ DA.deleteAt index suggestions
                              }
                  _ -> model

            nextSendMessage = do
                  date <- liftEffect $ map DateTimeWrapper EN.nowDateTime
                  CIF.next $ SendMessage date

      | otherwise =
            F.noMessages $ model {
                  message = Just content
            }

sendMessage :: WebSocket -> DateTimeWrapper -> IMModel -> NoMessages
sendMessage webSocket date = case _ of
      model@{
            user: { id: senderID },
            chatting: Just chatting,
            temporaryID,
            contacts,
            selectedImage,
            message,
            imageCaption
      } ->
            let  recipient@{ user: { id: recipientID }, history } = contacts !@ chatting
                 newTemporaryID = temporaryID + 1

                 updatedChatting = recipient {
                        history = DA.snoc history $  {
                              id: newTemporaryID,
                              status: Sent,
                              sender: senderID,
                              recipient: recipientID,
                              date,
                              content: DM.maybe' (\_ -> SU.fromJust message) (asMarkdownImage imageCaption) selectedImage
                        }
                 }
                 updatedModel = model {
                        temporaryID = newTemporaryID,
                        imageCaption = Nothing,
                        selectedImage = Nothing,
                        shouldSendMessage = false,
                        message = if DM.isJust selectedImage then message else Nothing,
                        contacts = SU.fromJust $ DA.updateAt chatting updatedChatting contacts
                 }
                 turn = makeTurn updatedChatting senderID
            in
                  CIF.nothingNext updatedModel $ liftEffect do
                        CIS.scrollLastMessage
                        CIW.sendPayload webSocket $ OutgoingMessage {
                              id: newTemporaryID,
                              userID: recipientID,
                              content: asMessageContent message imageCaption selectedImage,
                              turn
                        }
      model -> F.noMessages model
      where asMarkdownImage imageCaption base64 = "![" <> DM.fromMaybe "" imageCaption  <> "](" <> base64 <> ")"
            asMessageContent message imageCaption selectedImage = DM.maybe' (\_ -> Text $ SU.fromJust message) (Image <<< Tuple (DM.fromMaybe "" imageCaption)) selectedImage

makeTurn :: Contact -> PrimaryKey -> Maybe Turn
makeTurn { chatStarter, chatAge, history } sender =
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
                  Just {
                        senderStats: {
                              characters: senderCharacters,
                              interest: senderCharacters / recipientCharacters
                        },
                        recipientStats: {
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
                  let sender = last.sender
                      recipient = beforeLast.recipient
                  pure $ sender == userID && recipient == userID

            sameSender entry anotherEntry = entry.sender ==  anotherEntry.sender
            countCharacters total ( { content }) = total + DSC.length content
            getDate = DN.unwrap <<< _.date

applyMarkup :: Markup -> IMModel -> MoreMessages
applyMarkup markup model@{ message } = model :> [liftEffect (Just <$> apply markup (DM.fromMaybe "" message))]
      where apply markup value = do
                  textarea <- SU.fromJust <<< WHHTA.fromElement <$> CCD.unsafeQuerySelector "#chat-input"
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

setMessage :: Maybe Int -> String -> IMModel -> NextMessage
setMessage cursor markdown model =
      CIF.nothingNext (model {
            message = Just markdown
      }) <<< liftEffect $
            case cursor of
                  Just position -> do
                        textarea <- SU.fromJust <<< WHHTA.fromElement <$> CCD.unsafeQuerySelector "#chat-input"
                        WHHEL.focus $ WHHTA.toHTMLElement textarea
                        WHHTA.setSelectionEnd position textarea
                  Nothing -> pure unit

selectImage :: IMModel -> NextMessage
selectImage model = CIF.nothingNext model $ liftEffect do
      input <- getFileInput
      CCF.triggerFileSelect input

catchFile :: FileReader -> Event -> IMModel -> NoMessages
catchFile fileReader event model = CIF.nothingNext model $ liftEffect do
      CCF.readBase64 fileReader <<< WHEDT.files <<< WHED.dataTransfer <<< SU.fromJust $ WHED.fromEvent event
      CCD.preventStop event

toggleMessageEnter :: IMModel -> NoMessages
toggleMessageEnter model@{ messageEnter } =
      F.noMessages $ model {
            messageEnter = not messageEnter
      }

setSelectedImage :: Maybe String -> IMModel -> NoMessages
setSelectedImage maybeBase64 model =
      F.noMessages $ model {
            toggleChatModal = ShowSelectedImage,
            selectedImage = maybeBase64,
            erroredFields =
                  if isTooLarge $ DM.fromMaybe "" maybeBase64 then
                        [TDS.reflectSymbol (SProxy :: SProxy "selectedImage")]
                   else
                        []
      }
      where isTooLarge contents = maxImageSize < 3 * DI.ceil (DI.toNumber (DS.length contents) / 4.0)

toggleModal :: ShowChatModal -> IMModel -> MoreMessages
toggleModal toggle model = model {
      toggleChatModal = toggle,
      link = Nothing,
      selectedImage = Nothing,
      linkText = Nothing
} :> if toggle == ShowSelectedImage then [pickImage] else []
      where pickImage = liftEffect do
                  input <- getFileInput
                  CCF.triggerFileSelect input
                  pure Nothing

setEmoji :: Event -> IMModel -> NextMessage
setEmoji event model@{ message } = model {
      toggleChatModal = HideChatModal
} :> [liftEffect do
      emoji <- CCD.innerTextFromTarget event
      setAtCursor message emoji
]

insertLink :: IMModel -> MoreMessages
insertLink model@{ message, linkText, link } =
      case link of
            Nothing -> F.noMessages $ model {
                  erroredFields = [ TDS.reflectSymbol (SProxy :: SProxy "link") ]
            }
            Just url ->
                  let { protocol } = NU.parse $ DS.trim url
                  in model {
                        erroredFields = []
                  } :> [
                        CIF.next $ ToggleChatModal HideChatModal,
                        insert $ if protocol == null then "http://" <> url else url
                  ]
      where markdown url = "[" <> DM.fromMaybe url linkText <> "](" <> url <> ")"
            insert = liftEffect <<< setAtCursor message <<< markdown

setAtCursor :: Maybe String -> String -> Effect (Maybe IMMessage)
setAtCursor message text = do
      textarea <- SU.fromJust <<< WHHTA.fromElement <$> CCD.unsafeQuerySelector "#chat-input"
      end <- WHHTA.selectionEnd textarea
      let { before, after } = DS.splitAt end $ DM.fromMaybe "" message
      CIF.next <<< SetMessageContent (Just $ end + DS.length text + 1) $ before <> text <> after