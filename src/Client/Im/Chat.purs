module Client.Im.Chat where

import Prelude

import Client.AppId (imAppId)
import Client.Ui.Document as CUD
import Client.Ui.Element as CUE
import Client.Ui.Event as CUEV
import Client.Ui.Media as CUM
import Client.File as CF
import Client.Im.Flame (NextMessage, NoMessages, MoreMessages)
import Client.Im.Record as CIR
import Client.Im.Scroll as CIS
import Client.Im.Suggestion as CIS
import Client.Im.WebSocket as CIW
import Client.Im.WebSocket.Connection (WebSocket)
import Client.Network (routes)
import Client.Network as CN
import Data.Array ((!!), (:))
import Data.Array as DA
import Data.Array.NonEmpty as DAN
import Data.DateTime (DateTime)
import Data.DateTime as DD
import Data.Either (Either(..))
import Data.Int as DI
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Ord as DO
import Data.String (Pattern(..))
import Data.String as DS
import Data.String.CodeUnits as DSC
import Data.Symbol as DS
import Data.Time.Duration (Milliseconds(..), Minutes(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class as EC
import Effect.Now as EN
import Flame as F
import Flame.Html.Event (Event)
import Safe.Coerce as SC
import Shared.Content (Content(..))
import Shared.DateTime (DateTimeWrapper(..))
import Shared.DateTime as SD
import Shared.Element (ElementId(..))
import Shared.Element as SE
import Shared.Im.Contact as SIC
import Shared.Im.Types (Contact, ImMessage(..), ImModel, MessageStatus(..), RetryableRequest(..), SelectedImage, ShowContextMenu(..), TimeoutIdWrapper, Touch, Turn, WebSocketConnectionStatus(..), WebSocketPayloadServer(..))
import Shared.Markdown (Token(..))
import Shared.Markdown as SM
import Shared.Modal (ChatModal(..), Modal(..))
import Shared.ResizeInput as SR
import Shared.Resource (maxImageSize)
import Shared.Unsafe as SU
import Shared.User (User)
import Type.Proxy (Proxy(..))
import Web.DOM (Element)
import Web.DOM.Element as WDE

-- | Send a message on enter
enterSendMessage ∷ ElementId → Event → ImModel → NoMessages
enterSendMessage elementId event model =
      model /\ [ prevent, messageContent elementId model ]
      where
      prevent = EC.liftEffect do
            CUEV.preventDefault event
            pure Nothing

-- | Send a message on button click
forceSendMessage ∷ ElementId → ImModel → MoreMessages
forceSendMessage elementId model =
      model /\
            [ messageContent elementId model
            ]

--the annoying here thing that we need a better design over imperatively getting input element values
-- is that we need to support both keydown enter / button press to send messages, as well typing status and
-- text insertion and focus
-- | Is the message to be sent an audio, text or image?
messageContent ∷ ElementId → ImModel → Aff (Maybe ImMessage)
messageContent elementId model = do
      input ← EC.liftEffect $ CUD.unsafeGetElementById elementId
      value ← CUE.value input
      date ← EC.liftEffect $ map DateTimeWrapper EN.nowDateTime
      cleanUp input
      pure <<< Just $ SendMessage elementId (DM.maybe (Text value) toImage model.selectedImage) date
      where
      toImage selected = Image (DM.fromMaybe "" model.imageCaption) selected.width selected.height selected.base64

      cleanUp input = do
            CUE.focus input
            CUE.setValue input ""
            EC.liftEffect $ SR.resizeInput input

-- | Prepare and send a message via web socket
prepareSendMessage ∷ ElementId → Content → DateTimeWrapper → WebSocket → ImModel → MoreMessages
prepareSendMessage elementId content dt webSocket model = case content of
      Text message | DS.null $ DS.trim message → F.noMessages model
      _ → sendMessage (SU.fromJust updatedModel.chatting) model.user.name shouldFetchHistory content dt webSocket updatedModel
      where
      --the user messaged could be in both the contacts and suggestions
      -- in either case, check if the chat history has not already been fetched
      shouldFetchHistory /\ updatedModel = case elementId of
            ChatInput → Tuple false model
            _ →
                  let
                        user = SU.fromJust $ model.suggesting >>= (\suggestionId → DA.find ((suggestionId == _) <<< _.id) model.suggestions)
                  in
                        case SIC.findContact user.id model.contacts of
                              Nothing →
                                    Tuple true model
                                          { chatting = Just user.id
                                          , contacts = SIC.defaultContact model.user.id user : model.contacts
                                          , suggestions = DA.filter ((user.id /= _) <<< _.id) model.suggestions
                                          }
                              Just contact →
                                    Tuple contact.shouldFetchChatHistory model
                                          { chatting = Just contact.user.id
                                          , suggestions = DA.filter ((user.id /= _) <<< _.id) model.suggestions
                                          , suggesting = CIS.moveSuggestion model 1 --move it along so mini suggestions dont disappear
                                          }

sendMessage ∷ Int → String → Boolean → Content → DateTimeWrapper → WebSocket → ImModel → NoMessages
sendMessage userId userName shouldFetchHistory contentMessage date webSocket model =
      model
            { temporaryId = newTemporaryId
            , contacts = map updateContact model.contacts
            , imageCaption = Nothing
            , modal = HideModal
            , selectedImage = Nothing
            , showMiniChatInput = false
            , editing = Nothing
            } /\ ([ actuallySendMessage ] <> if shouldFetchHistory then [ fetchHistory ] else [])
      where
      -- temporary ids are used to find messages while the server has not yet returned their db id
      newTemporaryId = model.temporaryId + 1

      fetchHistory = pure <<< Just <<< SpecialRequest $ FetchHistory userId true
      actuallySendMessage = EC.liftEffect do
            CIS.scrollLastMessage
            let
                  payload = case model.editing of
                        Nothing →
                              OutgoingMessage
                                    { id: newTemporaryId
                                    , userId
                                    , userName
                                    , content: contentMessage
                                    , turn: makeTurn model.user <<< SU.fromJust $ SIC.findContact userId model.contacts
                                    }
                        Just messageId →
                              EditedMessage
                                    { id: messageId
                                    , userId
                                    , content: contentMessage
                                    }
            if model.webSocketStatus == Connected then do
                  CIW.sendPayload webSocket payload
                  pure Nothing
            else
                  pure <<< Just <<< ResumeWebSocketMessage $ Just payload

      asMarkdownImage caption width height base64 = "![" <> caption <> "]([" <> show width <> "," <> show height <> "]" <> base64 <> ")"
      asAudioMessage base64 = "<audio controls src='" <> base64 <> "'></audio>"
      markdown =
            case contentMessage of
                  Text txt → txt
                  Image caption width height base64File → asMarkdownImage caption width height base64File
                  Audio base64 → asAudioMessage base64
                  _ → ""
      updateContact contact
            | contact.user.id == userId =
                    contact
                          { lastMessageDate = date
                          , draft = ""
                          , history =
                                  case model.editing of
                                        Nothing →
                                              DA.snoc contact.history
                                                    { id: newTemporaryId
                                                    , status: Sent
                                                    , sender: model.user.id
                                                    , edited: false
                                                    , reaction: Nothing
                                                    , recipient: userId
                                                    , content: markdown
                                                    , date
                                                    }
                                        Just messageId → map (updateEdited messageId) contact.history
                          }
            | otherwise = contact
      updateEdited messageId history
            | history.id == messageId =
                    history
                          { content = markdown
                          , status = Sent
                          , edited = true
                          }
            | otherwise = history

-- | A "turn" is how much karma is accrued between messages
-- |
-- | See `Wheel.purs`
makeTurn ∷ User → Contact → Maybe Turn
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
                              SC.coerce (DD.diff (getDate $ DAN.head senderMessages) (getDate $ DAN.last previousRecipientMessages) ∷ Minutes)
                        recipientReplyDelay =
                              SC.coerce (DD.diff (getDate $ DAN.head recipientMessages) (getDate $ DAN.last senderMessages) ∷ Minutes)
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

      getDate = SC.coerce <<< _.date
      accountAge { joined: DateTimeWrapper dt } = DI.toNumber $ SD.daysDiff dt

-- | Find the cursor on the chatting textarea and set text at is position
setAtCursor ∷ Element → String → Aff (Maybe ImMessage)
setAtCursor input text = do
      end ← CUE.selectionEnd input
      value ← CUE.value input
      let { before, after } = DS.splitAt end value
      setTextAt input (end + DS.length text + 1) $ before <> text <> after
      pure Nothing

setTextAt ∷ Element → Int → String → Aff Unit
setTextAt input position markdown = do
      CUE.setValue input markdown
      CUE.setSelection input position
      CUE.focus input
      EC.liftEffect $ SR.resizeInput input

-- | Handle drag and drop
catchFile ∷ Event → ImModel → NoMessages
catchFile event model = model /\ [ catchIt ]
      where
      catchIt = EC.liftEffect do
            CF.compressImageFromFileList (CUEV.files event) imAppId false $ \width height base64 → SetSelectedImage $ Just { width, height, base64 }
            CUEV.preventStop event
            pure Nothing

-- | Handle file input selection
setSelectedImage ∷ SelectedImage → ImModel → NextMessage
setSelectedImage selected model =
      model
            { modal = Chat ShowSelectedImage
            , selectedImage = selected
            , erroredFields =
                    if isTooLarge $ DM.maybe "" _.base64 selected then
                          [ DS.reflectSymbol (Proxy ∷ Proxy "selectedImage") ]
                    else
                          []
            } /\ (if model.smallScreen then [] else [ pure <<< Just $ FocusInput ImageFormCaption ])
      where
      isTooLarge contents = maxImageSize < CF.fileSize contents

-- | Insert an emoji into the chatting textarea
setEmoji ∷ ElementId → Event → ImModel → NextMessage
setEmoji elementId event model = model /\ [ setIt, hideModal ]
      where
      setIt = do
            emoji ← EC.liftEffect $ CUEV.innerTextFromTarget event
            input ← EC.liftEffect $ CUD.unsafeGetElementById elementId
            setAtCursor input emoji
      hideModal = pure <<< Just <<< SpecialRequest $ ToggleModal HideModal

-- | Send "is typing" notification
sendTyping ∷ String → DateTime → WebSocket → ImModel → MoreMessages
sendTyping text now webSocket model =
      if DS.length text > minimumLength && enoughTime model.lastTyping then
            model { lastTyping = DateTimeWrapper now } /\ [ setIt ]
      else
            F.noMessages model
      where
      minimumLength = 7
      enoughTime (DateTimeWrapper dt) = let (Milliseconds ms) = DD.diff now dt in ms >= 800.0

      setIt = do
            EC.liftEffect <<< CIW.sendPayload webSocket $ Typing { id: SU.fromJust model.chatting }
            pure Nothing

-- | Show or hide typing status
toggleTyping ∷ Int → Maybe TimeoutIdWrapper → ImModel → ImModel
toggleTyping userId tId model = model { contacts = upd <$> model.contacts }
      where
      upd contact
            | contact.user.id == userId = contact { typing = DM.isJust tId, typingId = tId }
            | otherwise = contact

toggleMessageEnter ∷ ImModel → NoMessages
toggleMessageEnter model = F.noMessages model
      { messageEnter = not model.messageEnter
      }

toggleMiniChatInput ∷ ImModel → NoMessages
toggleMiniChatInput model = F.noMessages model
      { showMiniChatInput = not model.showMiniChatInput
      }

-- | Show or hide chat modals
modal ∷ ChatModal → ImModel → MoreMessages
modal toggle model =
      model
            { modal = Chat toggle
            , selectedImage = Nothing
            } /\ case toggle of
            ShowSelectedImage → [ pickImage ]
            _ → []
      where
      pickImage = EC.liftEffect do
            fileInput ← CUD.unsafeGetElementById ImageFileInput
            CF.triggerFileSelect fileInput
            pure Nothing

prepareSelectedImage ∷ Event → ImModel → MoreMessages
prepareSelectedImage event model = model /\ [ before ]
      where
      before = do
            CF.compressImage imAppId event false $ \width height base64 → SetSelectedImage $ Just { width, height, base64 }
            pure Nothing

-- | Record an audio message
beforeAudioMessage ∷ ImModel → MoreMessages
beforeAudioMessage model = model /\ [ record ]
      where
      record = EC.liftEffect do
            mimeType ← CUM.acceptedAudioCodec
            CIR.start { audio: true } { mimeType } SendAudioMessage
            pure Nothing

-- | Finish recording an audio message
audioMessage ∷ Touch → ImModel → MoreMessages
audioMessage touch model =
      model { modal = HideModal } /\
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
            pure <<< Just $ SendMessage ChatInput (Audio base64) date

-- | Messages can be quote from context menu, double click on desktop and swipe on mobile
quoteMessage ∷ Int → String → Either Touch (Maybe Event) → ImModel → NextMessage
quoteMessage messageId contents touchEvent model =
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
            Nothing → "> [" <> SE.messageElementId messageId <> "] *quote*"
            Just (Token token) → "> [" <> SE.messageElementId messageId <> "] " <> token.raw
      notSpaceQuote (Token token) = token."type" /= "space" && token."type" /= "blockquote"

      fromDoubleClick event = do
            classes ← EC.liftEffect <<< WDE.className <<< SU.fromJust $ do
                  target ← CUEV.target event
                  WDE.fromEventTarget target
            if DS.contains (Pattern "message") classes then
                  quoteIt
            else
                  pure Nothing

      fromSwipe touch
            | touch.startX < touch.endX && touch.endX - touch.startX >= threshold && DO.abs (touch.startY - touch.endY) < threshold = quoteIt
            | otherwise = pure Nothing

      quoteIt = do
            input ← EC.liftEffect $ CUD.unsafeGetElementById ChatInput
            let markup = sanitized <> "\n\n"
            value ← CUE.value input
            setAtCursor input $ if DS.null value then markup else "\n" <> markup

editMessage ∷ String → Int → ImModel → NoMessages
editMessage message id model =
      model
            { editing = Just id
            , toggleContextMenu = HideContextMenu
            } /\ [ setIt *> pure Nothing ]
      where
      setIt = do
            input ← EC.liftEffect $ CUD.unsafeGetElementById ChatInput
            CUE.setValue input message
            CUE.focus input

deleteMessage ∷ Int → WebSocket → ImModel → NoMessages
deleteMessage id webSocket model =
      model
            { toggleContextMenu = HideContextMenu
            } /\ [ deleteIt ]
      where
      deleteIt = do
            EC.liftEffect <<< CIW.sendPayload webSocket $ DeletedMessage { id, userId: SU.fromJust model.chatting }
            pure Nothing

messageFromExperiment ∷ Int → String → WebSocket → ImModel → MoreMessages
messageFromExperiment userId message webSocket model =
      if isContact then
            model /\ [ hide, resume, send ]
      else if isSuggestion then
            model { suggesting = Just userId } /\ [ hide, send ]
      else
            model { temporaryId = model.temporaryId + 1 } /\ [ hide, sendFetch, resume ]
      where
      isContact = DA.any ((userId == _) <<< _.id <<< _.user) model.contacts
      isSuggestion = DA.any ((userId == _) <<< _.id) model.suggestions

      hide = pure <<< Just <<< SpecialRequest $ ToggleModal HideModal
      resume = pure <<< Just $ ResumeChat userId
      send = do
            dt ← EC.liftEffect EN.nowDateTime
            pure <<< Just $ SendMessage (if isContact then ChatInput else ExperimentsRoot) (Text message) (DateTimeWrapper dt)
      sendFetch = do
            EC.liftEffect <<< CIW.sendPayload webSocket $ OutgoingMessage
                  { id: model.temporaryId + 1
                  , userId
                  , userName: model.user.name
                  , content: Text message
                  , turn: Nothing
                  }
            CN.retryableRequest FetchMissedContacts DisplayNewContacts $ routes.im.contact { query: { id: userId } }
