module Client.Im.Chat where

import Prelude
import Shared.Experiments.Types
import Shared.Im.Types

import Client.Common.Dom as CCD
import Client.Common.File as CCF
import Client.Im.Flame (MoreMessages, NextMessage, NoMessages)
import Client.Im.Flame as CIF
import Client.Im.Scroll as CIS
import Client.Im.WebSocket as CIW
import Control.Alt ((<|>))
import Data.Array ((!!))
import Data.Array as DA
import Data.Array.NonEmpty as DAN
import Data.DateTime (DateTime(..))
import Data.DateTime as DT
import Data.Int as DI
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Newtype as DN
import Data.Nullable (null)
import Data.String (Pattern(..))
import Data.String as DS
import Data.String.CodeUnits as DSC
import Data.Symbol as TDS
import Data.Time.Duration (Days(..), Milliseconds(..), Minutes(..), Seconds)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Debug (spy)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Now as EN
import Effect.Uncurried (EffectFn1)
import Effect.Uncurried as EU
import Flame ((:>))
import Flame as F
import Node.URL as NU
import Shared.DateTime (DateTimeWrapper(..))
import Shared.DateTime as SDT
import Shared.Element (ElementId(..))
import Shared.Im.Contact as SIC
import Shared.Markdown as SM
import Shared.Options.File (maxImageSize)
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
import Web.HTML.HTMLElement as WHHEL
import Web.HTML.HTMLTextAreaElement as WHHTA
import Web.Socket.WebSocket (WebSocket)

foreign import resizeTextarea_ ∷ EffectFn1 Element Unit

-- this event is filtered to run only on Enter keydown
enterBeforeSendMessage ∷ Event → ImModel → NoMessages
enterBeforeSendMessage event model@{ messageEnter } =
      model :> if messageEnter then [ prevent, getMessage model ] else []
      where
      prevent = liftEffect do
            WEE.preventDefault event
            pure Nothing

getMessage ∷ ImModel → Aff (Maybe ImMessage)
getMessage model@{ selectedImage, imageCaption, chatting } = do
      input ← liftEffect $ chatInput chatting
      value ← liftEffect $ CCD.value input
      pure <<< Just <<< BeforeSendMessage $ DM.maybe (Text value) toImage selectedImage
      where
      toImage base64File = Image (DM.fromMaybe "" imageCaption) base64File

--send message/image button
forceBeforeSendMessage ∷ ImModel → MoreMessages
forceBeforeSendMessage model =
      model :>
            [ getMessage model
            ]

beforeSendMessage ∷ MessageContent → ImModel → MoreMessages
beforeSendMessage
      content
      model@
            { chatting
            , user: { id }
            , contacts
            , suggesting
            , suggestions
            } = case content of
      t@(Text message)
            | isEmpty message → F.noMessages model
            | otherwise → updatedModel :> nextEffects t
      image → updatedModel :> nextEffects image

      where
      isEmpty = DS.null <<< DS.trim

      Tuple shouldFetchHistory updatedModel = case chatting, suggesting of
            Nothing, (Just index) →
                  --an existing contact might be in the suggestions
                  let
                        user = suggestions !@ index
                        maybeIndex = DA.findIndex (\cnt → cnt.user.id == user.id && cnt.impersonating == Nothing) contacts
                        updatedContacts = if DM.isJust maybeIndex then contacts else DA.cons (SIC.defaultContact id user) contacts
                        updatedChatting = maybeIndex <|> Just 0
                        shouldFetchHistory = _.shouldFetchChatHistory $ SU.fromJust do
                              index ← updatedChatting
                              updatedContacts !! index
                  in
                        Tuple shouldFetchHistory model
                              { chatting = updatedChatting
                              , contacts = updatedContacts
                              , suggestions = SU.fromJust $ DA.deleteAt index suggestions
                              }
            _, _ → Tuple false model

      nextEffects c = [ fetchHistory, nextSendMessage c ]

      fetchHistory = pure <<< Just <<< SpecialRequest $ FetchHistory shouldFetchHistory
      nextSendMessage input = do
            date ← liftEffect $ map DateTimeWrapper EN.nowDateTime
            CIF.next $ SendMessage input date

sendMessage ∷ WebSocket → MessageContent → DateTimeWrapper → ImModel → NoMessages
sendMessage
      webSocket
      content
      date
      model@
            { user
            , chatting
            , temporaryId
            , contacts
            , experimenting
            } = CIF.nothingNext updatedModel $ liftEffect do
      CIS.scrollLastMessage
      input ← chatInput chatting
      WHHEL.focus <<< SU.fromJust $ WHHEL.fromElement input
      CCD.setValue input ""
      CIW.sendPayload webSocket $ OutgoingMessage
            { id: newTemporaryId
            , userId: recipientId
            , content
            , experimenting: case experimenting, recipient.impersonating of
                    Just (Impersonation (Just { id })), _ → Just $ ImpersonationPayload { id: id, sender: true }
                    _, Just id → Just $ ImpersonationPayload { id: id, sender: false }
                    _, _ → Nothing
            , turn
            }
      where
      index = SU.fromJust chatting
      recipient@{ user: { id: recipientId }, history } = contacts !@ index
      newTemporaryId = temporaryId + 1

      updatedContact = recipient
            { lastMessageDate = date
            , history = DA.snoc history $
                    { id: newTemporaryId
                    , status: Sent
                    , sender: user.id
                    , recipient: recipientId
                    , date
                    , content: case content of
                            Text message → message
                            Image caption base64File → asMarkdownImage caption base64File
                    }
            }
      updatedModel = model
            { temporaryId = newTemporaryId
            , imageCaption = Nothing
            , selectedImage = Nothing
            , contacts = SU.fromJust $ DA.updateAt index updatedContact contacts
            }
      turn = makeTurn user updatedContact

      asMarkdownImage caption base64 = "![" <> caption <> "](" <> base64 <> ")"

makeTurn ∷ ImUser → Contact → Maybe Turn
makeTurn user@{ id } contact@{ chatStarter, chatAge, history } =
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
                              , chatAge
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

                              , chatAge
                              }
            _ → Nothing --not a turn
      where
      grouped
            | chatStarter == id && isNewTurn = DA.takeEnd 4 $ DA.groupBy sameSender history
            | otherwise = []
      isNewTurn = DM.fromMaybe false do
            last ← DA.last history
            beforeLast ← history !! (DA.length history - 2)
            pure $ last.sender == id && beforeLast.recipient == id
      sameSender entry anotherEntry = entry.sender == anotherEntry.sender

      characters = DI.toNumber <<< DA.foldl countCharacters 0 <<< DAN.toArray
      countCharacters total { content } = total + DSC.length content

      getDate = DN.unwrap <<< _.date

      accountAge { joined: DateTimeWrapper dt } = DN.unwrap (DT.diff SDT.unsafeNow dt ∷ Days)

applyMarkup ∷ Markup → ImModel → MoreMessages
applyMarkup markup model@{ chatting } =
      model :>
            [ liftEffect (Just <$> apply)
            ]
      where
      apply = do
            input ← chatInput chatting
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
            pure $ SetMessageContent (Just $ end + beforeSize) newValue

      plusNewLine value t
            | DS.null value = t
            | otherwise = "\n" <> t

chatInput ∷ Maybe Int → Effect Element
chatInput chatting
      | DM.isNothing chatting = CCD.unsafeGetElementById ChatInputSuggestion -- suggestion card input cannot be cached
      | otherwise = CCD.unsafeGetElementById ChatInput

setMessage ∷ Maybe Int → String → ImModel → NextMessage
setMessage cursor markdown model@{ chatting } =
      CIF.nothingNext model <<< liftEffect $
            case cursor of
                  Just position → do
                        input ← chatInput chatting
                        let textarea = SU.fromJust $ WHHTA.fromElement input
                        WHHEL.focus $ WHHTA.toHTMLElement textarea
                        WHHTA.setValue markdown textarea
                        WHHTA.setSelectionEnd position textarea
                  Nothing → pure unit

catchFile ∷ FileReader → Event → ImModel → NoMessages
catchFile fileReader event model = CIF.nothingNext model $ liftEffect do
      CCF.readBase64 fileReader <<< WHEDT.files <<< WHED.dataTransfer <<< SU.fromJust $ WHED.fromEvent event
      CCD.preventStop event

setSelectedImage ∷ Maybe String → ImModel → NextMessage
setSelectedImage maybeBase64 model@{ smallScreen } =
      model
            { toggleChatModal = ShowSelectedImage
            , selectedImage = maybeBase64
            , erroredFields =
                    if isTooLarge $ DM.fromMaybe "" maybeBase64 then
                          [ TDS.reflectSymbol (Proxy ∷ Proxy "selectedImage") ]
                    else
                          []
            } :> (if smallScreen then [] else [ CIF.next $ FocusInput ImageFormCaption ])
      where
      isTooLarge contents = maxImageSize < 3 * DI.ceil (DI.toNumber (DS.length contents) / 4.0)

toggleModal ∷ ShowChatModal → ImModel → MoreMessages
toggleModal toggle model@{ chatting } =
      model
            { toggleChatModal = toggle
            , link = Nothing
            , selectedImage = Nothing
            , linkText = Nothing
            } :> case toggle of
            ShowSelectedImage → [ pickImage ]
            ShowLinkForm → [ CIF.next $ FocusInput LinkFormUrl ]
            ShowPreview → [ setPreview ]
            _ → []
      where
      pickImage = liftEffect do
            fileInput ← CCD.unsafeGetElementById ImageFileInput
            CCF.triggerFileSelect fileInput
            pure Nothing

      setPreview = liftEffect do
            preview ← CCD.unsafeGetElementById ChatInputPreview
            input ← chatInput chatting
            message ← CCD.value input
            CCD.setInnerHTML preview $ SM.parse message
            pure Nothing

setEmoji ∷ Event → ImModel → NextMessage
setEmoji event model@{ chatting } =
      model :>
            if CCD.tagNameFromTarget event == "SPAN" then
                  [ liftEffect do
                          emoji ← CCD.innerTextFromTarget event
                          input ← chatInput chatting
                          setAtCursor input emoji
                  , pure <<< Just $ ToggleChatModal HideChatModal
                  ]
            else
                  []

insertLink ∷ ImModel → MoreMessages
insertLink model@{ linkText, link, chatting } =
      case link of
            Nothing → F.noMessages $ model
                  { erroredFields = [ TDS.reflectSymbol (Proxy ∷ _ "link") ]
                  }
            Just url →
                  let
                        { protocol } = NU.parse $ DS.trim url
                  in
                        model
                              { erroredFields = []
                              } :>
                              [ CIF.next $ ToggleChatModal HideChatModal
                              , insert $ if protocol == null then "http://" <> url else url
                              ]
      where
      markdown url = "[" <> DM.fromMaybe url linkText <> "](" <> url <> ")"
      insert text = liftEffect do
            input ← chatInput chatting
            setAtCursor input $ markdown text

setAtCursor ∷ Element → String → Effect (Maybe ImMessage)
setAtCursor input text = do
      let textarea = SU.fromJust $ WHHTA.fromElement input
      end ← WHHTA.selectionEnd textarea
      value ← WHHTA.value textarea
      let { before, after } = DS.splitAt end value
      CIF.next <<< SetMessageContent (Just $ end + DS.length text + 1) $ before <> text <> after

resizeTextarea ∷ Element → Effect Unit
resizeTextarea = EU.runEffectFn1 resizeTextarea_

resizeChatInput ∷ Event → ImModel → NextMessage
resizeChatInput event model = CIF.nothingNext model resize
      where
      resize = liftEffect <<< resizeTextarea <<< SU.fromJust $ do
            target ← WEE.target event
            WDE.fromEventTarget target

toggleMessageEnter ∷ ImModel → NoMessages
toggleMessageEnter model@{ messageEnter } = F.noMessages $ model
      { messageEnter = not messageEnter
      }

checkTyping ∷ String → DateTime → WebSocket → ImModel → MoreMessages
checkTyping text now webSocket model@{ lastTyping: DateTimeWrapper lt, contacts, chatting } =
      if DS.length text > minimumLength && milliseconds >= 150.0 && milliseconds <= 1000.0 then
            CIF.nothingNext (model { lastTyping = DateTimeWrapper now }) <<< liftEffect <<< CIW.sendPayload webSocket $ Typing { id: (SU.fromJust (chatting >>= (contacts !! _))).user.id }
      else
            F.noMessages model { lastTyping = DateTimeWrapper now }
      where
      minimumLength = 7
      (Milliseconds milliseconds) = DT.diff now lt

quoteMessage ∷ String → Event → ImModel → NextMessage
quoteMessage contents event model@{ chatting } = model :>
      [ liftEffect do
              classes ← WDE.className <<< SU.fromJust $ do
                    target ← WEE.target event
                    WDE.fromEventTarget target
              if DS.contains (Pattern "message") classes then do
                    input ← chatInput chatting
                    let markup = sanitized <> "\n\n"
                    value ← WHHTA.value <<< SU.fromJust $ WHHTA.fromElement input
                    setAtCursor input $ if DS.null value then markup else "\n" <> markup
              else
                    pure Nothing
      ]
      where
      sanitized = case DA.find notSpaceQuote $ SM.lexer contents of
            Nothing → "> *quote*"
            Just token → "> " <> token.raw

      notSpaceQuote token = token."type" /= "space" && token."type" /= "blockquote"

focusCurrentSuggestion ∷ ImModel → NoMessages
focusCurrentSuggestion model@{ chatting } = model :>
      [ do
              liftEffect do
                    input ← chatInput chatting
                    WHHE.focus <<< SU.fromJust $ WHHE.fromElement input
              pure Nothing
      ]

updateTyping ∷ Int → Boolean → ImModel → ImModel
updateTyping userId status model@{ contacts } = model { contacts = upd <$> contacts }
      where
      upd contact@{ user: { id } }
            | id == userId = contact { typing = status }
            | otherwise = contact
