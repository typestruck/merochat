module Client.Im.Chat where

import Prelude

import Client.Common.Dom as CCD
import Client.Common.File as CCF
import Client.Im.Flame (MoreMessages, NextMessage, NoMessages)
import Client.Im.Record as CIR
import Client.Im.Scroll as CIS
import Client.Im.WebSocket as CIW
import Control.Alt ((<|>))
import Data.Array ((!!), (:))
import Data.Array as DA
import Data.Array.NonEmpty as DAN
import Data.DateTime (DateTime)
import Data.DateTime as DT
import Data.Either (Either(..))
import Data.Int as DI
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Newtype as DN
import Data.Nullable (null)
import Data.String (Pattern(..))
import Data.String as DS
import Data.String.CodeUnits as DSC
import Data.Symbol as TDS
import Data.Time.Duration (Milliseconds(..), Minutes)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Debug (spy)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class as EC
import Effect.Now as EN
import Effect.Uncurried (EffectFn1)
import Effect.Uncurried as EU
import Flame as F
import Node.URL as NU
import Shared.DateTime (DateTimeWrapper(..))
import Shared.DateTime as ST
import Shared.Element (ElementId(..))
import Shared.Im.Contact as SIC
import Shared.Im.Types (Contact, ImMessage(..), ImModel, ImUser, Markup(..), MessageContent(..), MessageStatus(..), RetryableRequest(..), ShowChatModal(..), ShowContextMenu(..), Touch, Turn, WebSocketPayloadServer(..))
import Shared.Markdown (Token(..))
import Shared.Markdown as SM
import Shared.Resource (maxImageSize)
import Shared.Unsafe ((!@))
import Shared.Unsafe as SU
import Type.Proxy (Proxy(..))
import Web.DOM (Element)
import Web.DOM.Element as WDE
import Web.Event.Event (Event)
import Web.Event.Event as WEE
import Web.File.FileReader (FileReader)
import Web.HTML.Event.DataTransfer as WHEDT
import Web.HTML.Event.DragEvent as WHED
import Web.HTML.HTMLElement as WHHE
import Web.HTML.HTMLTextAreaElement as WHHTA
import Web.Socket.WebSocket (WebSocket)

foreign import resizeTextarea_ ∷ EffectFn1 Element Unit

resizeTextarea ∷ Element → Effect Unit
resizeTextarea = EU.runEffectFn1 resizeTextarea_

-- | The chatting textarea grows / shrink as text is inputed
resizeChatInput ∷ Event → ImModel → NoMessages
resizeChatInput event model = model /\ [ resize ]
      where
      resize = do
            EC.liftEffect <<< resizeTextarea <<< SU.fromJust $ do
                  target ← WEE.target event
                  WDE.fromEventTarget target
            pure Nothing

-- | Send a message on enter
enterBeforeSendMessage ∷ Event → ImModel → NoMessages
enterBeforeSendMessage event model =
      model /\ [ prevent, messageContent model ]
      where
      prevent = EC.liftEffect do
            WEE.preventDefault event
            pure Nothing

-- | Is the message to be sent an audio, text or image?
messageContent ∷ ImModel → Aff (Maybe ImMessage)
messageContent model = do
      input ← EC.liftEffect $ chatInput model.chatting
      value ← EC.liftEffect $ CCD.value input
      pure <<< Just <<< BeforeSendMessage $ DM.maybe (Text value) toImage model.selectedImage
      where
      toImage base64File = Image (DM.fromMaybe "" model.imageCaption) base64File

-- | Send a message on button click
forceBeforeSendMessage ∷ ImModel → MoreMessages
forceBeforeSendMessage model =
      model /\
            [ messageContent model
            ]

-- | Raise the `SendMessage` event and fetch chat history if messaging a suggestion user
beforeSendMessage ∷ MessageContent → ImModel → MoreMessages
beforeSendMessage content model = case content of
      txt@(Text message)
            | DS.null $ DS.trim message → F.noMessages model
            | otherwise → updatedModel /\ nextEffects txt
      image → updatedModel /\ nextEffects image

      where
      --the user messaged could be in the contacts and suggestions
      -- in either case, check if the chat history has not already been fetched
      shouldFetchHistory /\ updatedModel = case model.chatting of
            Nothing →
                  let
                        user = model.suggestions !@ model.suggesting
                        maybeIndex = DA.findIndex ((_ == user.id) <<< _.id <<< _.user) model.contacts
                        updatedContacts
                              | DM.isJust maybeIndex = model.contacts
                              | otherwise = SIC.defaultContact model.user.id user : model.contacts
                        updatedChatting = maybeIndex <|> Just 0
                  in
                        Tuple (updatedContacts !@ SU.fromJust updatedChatting).shouldFetchChatHistory model
                              { chatting = updatedChatting
                              , contacts = updatedContacts
                              , suggestions = SU.fromJust $ DA.deleteAt model.suggesting model.suggestions
                              }
            _ → Tuple false model

      nextEffects ct = [ nextSendMessage ct, fetchHistory ]

      fetchHistory = pure <<< Just <<< SpecialRequest $ FetchHistory shouldFetchHistory
      nextSendMessage ct = do
            date ← EC.liftEffect $ map DateTimeWrapper EN.nowDateTime
            pure <<< Just $ SendMessage ct date

-- | Send a message via web socket
sendMessage ∷ WebSocket → MessageContent → DateTimeWrapper → ImModel → NoMessages
sendMessage webSocket contentMessage date model =
      model
            { temporaryId = newTemporaryId
            , imageCaption = Nothing
            , selectedImage = Nothing
            , editing = Nothing
            , contacts = SU.fromJust $ DA.updateAt chatting updatedContact model.contacts
            } /\ [ actuallySendMessage ]
      where
      -- temporary ids are used to find messages while the server has not yet returned their db id
      newTemporaryId = model.temporaryId + 1
      chatting = SU.fromJust model.chatting
      recipient = model.contacts !@ chatting

      actuallySendMessage = EC.liftEffect do
            CIS.scrollLastMessage
            input ← chatInput model.chatting
            WHHE.focus <<< SU.fromJust $ WHHE.fromElement input
            CCD.setValue input ""
            resizeTextarea input
            CIW.sendPayload webSocket $
                  case model.editing of
                        Nothing →
                              OutgoingMessage
                                    { id: newTemporaryId
                                    , userId: recipient.user.id
                                    , content: contentMessage
                                    , turn: makeTurn model.user updatedContact
                                    }
                        Just messageId →
                              EditedMessage
                                    { id: messageId
                                    , userId: recipient.user.id
                                    , content: contentMessage
                                    }
            pure Nothing

      markdown =
            case contentMessage of
                  Text txt → txt
                  Image caption base64File → asMarkdownImage caption base64File
                  Audio base64 → asAudioMessage base64

      updatedContact = recipient
            { lastMessageDate = date
            , history =
                    case model.editing of
                          Nothing →
                                DA.snoc recipient.history $
                                      { id: newTemporaryId
                                      , status: Sent
                                      , sender: model.user.id
                                      , edited: false
                                      , recipient: recipient.user.id
                                      , content: markdown
                                      , date
                                      }
                          Just messageId → map (updateEdited messageId) recipient.history
            }

      updateEdited messageId history
            | history.id == messageId =
                    history
                          { content = markdown
                          , status = Sent
                          , edited = true
                          }
            | otherwise = history

      asMarkdownImage caption base64 = "![" <> caption <> "](" <> base64 <> ")"
      asAudioMessage base64 = "<audio controls src='" <> base64 <> "'></audio>"

-- | A "turn" is how much karma is accrued between messages
-- |
-- | See `Wheel.purs`
makeTurn ∷ ImUser → Contact → Maybe Turn
makeTurn user contact =
      case grouped of
            [ senderMessages, recipientMessages, _ ] → --turn but we can't calculate reply bonus
                  let
                        senderCharacters = characters senderMessages
                        recipientCharacters = characters recipientMessages
                  in
                        Just
                              { senderStats:
                                      { characters: senderCharacters
                                      , interest: Nothing
                                      , replyDelay: Nothing
                                      , accountAge: accountAge user
                                      }
                              , recipientStats:
                                      { characters: recipientCharacters
                                      , interest: Just $ recipientCharacters / senderCharacters
                                      , replyDelay: Nothing
                                      , accountAge: accountAge contact.user
                                      }
                              , chatAge: contact.chatAge
                              }
            [ previousRecipientMessages, senderMessages, recipientMessages, _ ] →
                  let
                        senderCharacters = characters senderMessages
                        recipientCharacters = characters recipientMessages
                        previousRecipientCharacters = characters previousRecipientMessages

                        senderReplyDelay =
                              DN.unwrap (DT.diff (getDate $ DAN.head senderMessages) (getDate $ DAN.last previousRecipientMessages) ∷ Minutes)
                        recipientReplyDelay =
                              DN.unwrap (DT.diff (getDate $ DAN.head recipientMessages) (getDate $ DAN.last senderMessages) ∷ Minutes)
                  in
                        Just
                              { senderStats:
                                      { characters: senderCharacters
                                      , interest: Just $ senderCharacters / previousRecipientCharacters
                                      , replyDelay: Just senderReplyDelay
                                      , accountAge: accountAge user
                                      }
                              , recipientStats:
                                      { characters: recipientCharacters
                                      , interest: Just $ recipientCharacters / senderCharacters
                                      , replyDelay: Just recipientReplyDelay
                                      , accountAge: accountAge contact.user
                                      }

                              , chatAge: contact.chatAge
                              }
            _ → Nothing --not a turn
      where
      grouped
            | contact.chatStarter == user.id && isNewTurn = DA.takeEnd 4 $ DA.groupBy sameSender contact.history
            | otherwise = []

      isNewTurn = DM.fromMaybe false do
            last ← DA.last contact.history
            beforeLast ← contact.history !! (DA.length contact.history - 2)
            pure $ last.sender == user.id && beforeLast.recipient == user.id

      sameSender entry anotherEntry = entry.sender == anotherEntry.sender

      characters = DI.toNumber <<< DA.foldl countCharacters 0 <<< DAN.toArray
      countCharacters total entry = total + DSC.length entry.content

      getDate = DN.unwrap <<< _.date
      accountAge { joined: DateTimeWrapper dt } = DI.toNumber $ ST.daysDiff dt

-- | Insert markdown text
setMarkup ∷ Markup → ImModel → MoreMessages
setMarkup markup model =
      model /\ [ setIt ]
      where
      setIt = EC.liftEffect do
            input ← chatInput model.chatting
            value ← CCD.value input
            let
                  textarea = SU.fromJust $ WHHTA.fromElement input
                  Tuple before after = case markup of
                        Bold → Tuple "**" "**"
                        Italic → Tuple "*" "*"
                        Strike → Tuple "~" "~"
                        Heading → Tuple (plusNewLine value "## ") ""
                        OrderedList → Tuple (plusNewLine value "1. ") ""
                        UnorderedList → Tuple (plusNewLine value "- ") ""
            start ← WHHTA.selectionStart textarea
            end ← WHHTA.selectionEnd textarea
            let
                  beforeSize = DS.length before
                  beforeSelection = DS.take start value
                  selected = DS.take (end - start) $ DS.drop start value
                  afterSelection = DS.drop end value
                  newValue = beforeSelection <> before <> selected <> after <> afterSelection
            setTextAt input (end + beforeSize) newValue
            pure Nothing

      plusNewLine value t
            | DS.null value = t
            | otherwise = "\n" <> t

-- | Return the current textarea used to type messages
chatInput ∷ Maybe Int → Effect Element
chatInput chatting
      | DM.isNothing chatting = CCD.unsafeGetElementById ChatInputSuggestion -- suggestion card input cannot be cached
      | otherwise = CCD.unsafeGetElementById ChatInput

-- | Find the cursor on the chatting textarea and set text at is position
setAtCursor ∷ Element → String → Effect (Maybe ImMessage)
setAtCursor input text = do
      let textarea = SU.fromJust $ WHHTA.fromElement input
      end ← WHHTA.selectionEnd textarea
      value ← WHHTA.value textarea
      let { before, after } = DS.splitAt end value
      setTextAt input (end + DS.length text + 1) $ before <> text <> after
      pure Nothing

-- | Set text on the chatting textarea at the given cursor position
setTextAt ∷ Element → Int → String → Effect Unit
setTextAt input position markdown = do
      let textarea = SU.fromJust $ WHHTA.fromElement input
      WHHE.focus $ WHHTA.toHTMLElement textarea
      WHHTA.setValue markdown textarea
      WHHTA.setSelectionEnd position textarea
      resizeTextarea input

-- | Handle drag and drop
catchFile ∷ FileReader → Event → ImModel → NoMessages
catchFile fileReader event model = model /\ [ catchIt ]
      where
      catchIt = EC.liftEffect do
            CCF.readBase64 fileReader <<< WHEDT.files <<< WHED.dataTransfer <<< SU.fromJust $ WHED.fromEvent event
            CCD.preventStop event
            pure Nothing

-- | Handle file input selection
setSelectedImage ∷ Maybe String → ImModel → NextMessage
setSelectedImage maybeBase64 model =
      model
            { toggleChatModal = ShowSelectedImage
            , selectedImage = maybeBase64
            , erroredFields =
                    if isTooLarge $ DM.fromMaybe "" maybeBase64 then
                          [ TDS.reflectSymbol (Proxy ∷ Proxy "selectedImage") ]
                    else
                          []
            } /\ (if model.smallScreen then [] else [ pure <<< Just $ FocusInput ImageFormCaption ])
      where
      isTooLarge contents = maxImageSize < 3 * DI.ceil (DI.toNumber (DS.length contents) / 4.0)

-- | Insert an emoji into the chatting textarea
setEmoji ∷ Event → ImModel → NextMessage
setEmoji event model = model /\ [ setIt, hideModal ]
      where
      setIt = EC.liftEffect do
            emoji ← CCD.innerTextFromTarget event
            input ← chatInput model.chatting
            setAtCursor input emoji
      hideModal = pure <<< Just $ ToggleChatModal HideChatModal

-- | Insert a markdown link into the chatting textarea
setLink ∷ ImModel → MoreMessages
setLink model =
      case model.link of
            Nothing → F.noMessages model
                  { erroredFields = [ TDS.reflectSymbol (Proxy ∷ _ "link") ]
                  }
            Just url →
                  model
                        { erroredFields = []
                        } /\
                        [ hide
                        , setIt $ if (NU.parse $ DS.trim url).protocol == null then "http://" <> url else url
                        ]
      where
      markdown url = "[" <> DM.fromMaybe url model.linkText <> "](" <> url <> ")"
      hide = pure <<< Just $ ToggleChatModal HideChatModal
      setIt text = EC.liftEffect do
            input ← chatInput model.chatting
            setAtCursor input $ markdown text

-- | Send "is typing" notification
sendTyping ∷ String → DateTime → WebSocket → ImModel → MoreMessages
sendTyping text now webSocket model =
      if DS.length text > minimumLength && enoughTime model.lastTyping then
            model { lastTyping = DateTimeWrapper now } /\ [ setIt ]
      else
            F.noMessages model
      where
      minimumLength = 7
      enoughTime (DateTimeWrapper dt) = let (Milliseconds ms) = DT.diff now dt in ms >= 800.0

      setIt = do
            EC.liftEffect <<< CIW.sendPayload webSocket $ Typing { id: (model.contacts !@ SU.fromJust model.chatting).user.id }
            pure Nothing

-- | Show or hide typing status
toggleTyping ∷ Int → Boolean → ImModel → ImModel
toggleTyping userId status model = model { contacts = upd <$> model.contacts }
      where
      upd contact
            | contact.user.id == userId = contact { typing = status }
            | otherwise = contact

toggleMessageEnter ∷ ImModel → NoMessages
toggleMessageEnter model = F.noMessages model
      { messageEnter = not model.messageEnter
      }

-- | Show or hide chat modals
toggleModal ∷ ShowChatModal → ImModel → MoreMessages
toggleModal toggle model =
      model
            { toggleChatModal = toggle
            , link = Nothing
            , selectedImage = Nothing
            , linkText = Nothing
            } /\ case toggle of
            ShowSelectedImage → [ pickImage ]
            ShowLinkForm → [ pure <<< Just $ FocusInput LinkFormUrl ]
            ShowPreview → [ setPreview ]
            _ → []
      where
      pickImage = EC.liftEffect do
            fileInput ← CCD.unsafeGetElementById ImageFileInput
            CCF.triggerFileSelect fileInput
            pure Nothing

      setPreview = EC.liftEffect do
            preview ← CCD.unsafeGetElementById ChatInputPreview
            input ← chatInput model.chatting
            message ← CCD.value input
            CCD.setInnerHTML preview $ SM.parse message
            pure Nothing

-- | Record an audio message
beforeAudioMessage ∷ ImModel → MoreMessages
beforeAudioMessage model = model /\ [ record ]
      where
      record = EC.liftEffect do
            mimeType ← CCD.acceptedAudioCodec
            CIR.start { audio: true } { mimeType } SendAudioMessage
            pure Nothing

-- | Finish recording an audio message
audioMessage ∷ Touch → ImModel → MoreMessages
audioMessage touch model =
      model { toggleChatModal = HideChatModal } /\
            [ finish
            ]

      where
      threshold = 20
      finish = do
            when (touch.startX - touch.endX <= threshold && touch.startY - touch.endY <= threshold) $ EC.liftEffect CIR.stop
            pure Nothing

sendAudioMessage ∷ String → ImModel → MoreMessages
sendAudioMessage base64 model =
      model /\ [ sendIt ]
      where
      sendIt = do
            date ← EC.liftEffect $ map DateTimeWrapper EN.nowDateTime
            pure <<< Just $ SendMessage (Audio base64) date

-- | Messages can be quote from context menu, double click on desktop and swipe on mobile
quoteMessage ∷ String → Either Touch (Maybe Event) → ImModel → NextMessage
quoteMessage contents touchEvent model =
      case touchEvent of
            Right Nothing →
                  model { toggleContextMenu = HideContextMenu } /\ [ quoteIt ]
            Right (Just event) →
                  model /\ [ fromDoubleClick event ]
            Left touch →
                  model /\ [ fromSwipe touch ]
      where
      threshold = 40
      sanitized = case DA.find notSpaceQuote $ SM.lexer contents of
            Nothing → "> *quote*"
            Just (Token token) → "> " <> token.raw
      notSpaceQuote (Token token) = token."type" /= "space" && token."type" /= "blockquote"

      fromDoubleClick event = do
            classes ← EC.liftEffect <<< WDE.className <<< SU.fromJust $ do
                  target ← WEE.target event
                  WDE.fromEventTarget target
            if DS.contains (Pattern "message") classes then
                  quoteIt
            else
                  pure Nothing

      fromSwipe touch
            | touch.startX < touch.endX && touch.endX - touch.startX >= threshold && touch.startY - touch.endY < threshold = quoteIt
            | otherwise = pure Nothing

      quoteIt = EC.liftEffect do
            input ← chatInput model.chatting
            let markup = sanitized <> "\n\n"
            value ← WHHTA.value <<< SU.fromJust $ WHHTA.fromElement input
            setAtCursor input $ if DS.null value then markup else "\n" <> markup

-- | Focus suggestions card when the desktop left suggestions banner is clicked
focusCurrentSuggestion ∷ ImModel → NoMessages
focusCurrentSuggestion model = model /\ [ focus ]
      where
      focus = EC.liftEffect do
            input ← chatInput model.chatting
            WHHE.focus <<< SU.fromJust $ WHHE.fromElement input
            pure Nothing

editMessage ∷ String → Int → ImModel → NoMessages
editMessage message id model =
      model
            { editing = Just id
            , toggleContextMenu = HideContextMenu
            } /\ [ setIt *> pure Nothing ]
      where
      setIt = EC.liftEffect do
            input ← chatInput model.chatting
            CCD.setValue (spy "inp" input) message

deleteMessage ∷ Int → WebSocket → ImModel → NoMessages
deleteMessage id webSocket model =
      model
            { toggleContextMenu = HideContextMenu
            } /\ [ deleteIt ]
      where
      chatting = SU.fromJust model.chatting

      deleteIt = do
            EC.liftEffect <<< CIW.sendPayload webSocket $ DeletedMessage { id, userId: (model.contacts !@ chatting).user.id }
            pure Nothing

