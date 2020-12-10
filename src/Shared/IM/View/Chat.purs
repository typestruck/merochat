module Shared.IM.View.Chat where

import Prelude
import Shared.Types

import Control.Alt ((<|>))
import Data.Array ((!!))
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String as DS
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Symbol as TDS
import Data.Tuple (Tuple(..))
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Prim.Row (class Cons)
import Shared.IM.Emoji as SIE
import Shared.Keydown as SK
import Shared.Markdown as SM
import Shared.Options.File (maxImageSizeKB)
import Shared.Setter as SS

chat :: IMModel -> Html IMMessage
chat model@{ chatting } =
      HE.div [HA.class' {"send-box" : true, "hidden": DM.isNothing chatting }, HA.tabindex 0, SK.keyDownOn "Escape" $ ToggleChatModal HideChatModal] [
            linkModal model,
            imageModal model,
            chatBarInput model
      ]

linkModal :: IMModel -> Html IMMessage
linkModal {toggleChatModal, linkText, link, erroredFields} =
      HE.div [HA.class' {"link-form modal-form": true, hidden: toggleChatModal /= ShowLinkForm }] [
            HE.label_ "Text",
            HE.input [HA.type' "text", HA.placeholder "optional title", HA.value $ DM.fromMaybe "" linkText, HA.onInput (setJust (SProxy :: SProxy "linkText"))],
            HE.label_ "Link",
            HE.input [HA.type' "text", HA.id "link-form-url", HA.placeholder "http://", HA.value $ DM.fromMaybe "" link, HA.onInput (setJust (SProxy :: SProxy "link"))],
            HE.span [HA.class' {"error-message": true, "invisible": not (DS.null (DM.fromMaybe "" link)) || not (DA.elem (TDS.reflectSymbol (SProxy :: SProxy "link")) erroredFields) }] "Please enter a link",
            HE.div (HA.class' "buttons") [
                  HE.button [HA.class' "cancel", HA.onClick $ ToggleChatModal HideChatModal] "Cancel",
                  HE.button [HA.class' "green-button", HA.onClick InsertLink] "Insert"
            ]
      ]

imageModal :: IMModel -> Html IMMessage
imageModal {selectedImage, erroredFields} =
      HE.div [HA.class' { "image-form modal-form": true, hidden: DM.isNothing selectedImage }] [
            HE.div (HA.class' { "upload-div": true, hidden : not imageValidationFailed }) [
                  HE.input [HA.id "image-file-input", HA.type' "file", HA.value "", HA.accept ".png, .jpg, .jpeg, .tif, .tiff, .bmp"],
                  HE.div (HA.class' "error-message") $ "Image is larger than the " <> maxImageSizeKB <> " limit. Please select a different file."
            ],
            HE.div (HA.class' { "image-form-image": true, hidden: imageValidationFailed }) [
                  HE.img <<< HA.src $ DM.fromMaybe "" selectedImage
            ],
            HE.label_ "Caption",
            HE.input [HA.placeholder "optional title", HA.id "image-form-caption", HA.type' "text", HA.onInput (setJust (SProxy :: SProxy "imageCaption"))],
            HE.div (HA.class' "image-buttons") [
                  HE.button [HA.class' "cancel", HA.onClick $ ToggleChatModal HideChatModal] "Cancel",
                  HE.svg [HA.class' "svg-50 send-image-button", HA.onClick ForceBeforeSendMessage, HA.viewBox "0 0 300 300"] [
                        HE.title "Send image",
                        HE.path' (HA.d "M150,278.5A128.5,128.5,0,1,1,278.5,150,128.64,128.64,0,0,1,150,278.5Zm0-256A127.5,127.5,0,1,0,277.5,150,127.65,127.65,0,0,0,150,22.5Z"),
                        HE.polygon' (HA.points "99.76 213.29 125.13 153.56 99.76 93.81 241.34 153.56 99.76 213.29")
                  ]
            ]
      ]
      where imageValidationFailed = DA.elem (TDS.reflectSymbol (SProxy :: SProxy "selectedImage")) erroredFields

chatBarInput :: IMModel -> Html IMMessage
chatBarInput model@{
      chatting,
      contacts,
      suggesting,
      isWebSocketConnected,
      message,
      messageEnter,
      suggestions,
      toggleChatModal
 } = HE.fragment [
      emojiModal model,
      if toggleChatModal == ShowPreview then
            HE.div (HA.class' { hidden: toggleChatModal /= ShowPreview }) [
                  HE.div [HA.class' "chat-input-options"] [
                        HE.button [HA.onClick $ ToggleChatModal HideChatModal, HA.title "Exit preview"] "Exit"
                  ],
                  HE.div' [HA.innerHtml <<< SM.parse $ DM.fromMaybe "" message]
            ]
       else
            HE.div [HA.class' { hidden: not available || DM.isNothing chatting && DM.isNothing suggesting }] [
                  HE.div [HA.class' "chat-input-options"] [
                        bold,
                        italic,
                        strikethrough,
                        heading,
                        unorderedList,
                        orderedList,
                        linkButton toggleChatModal,
                        HE.button [HA.class' "preview", HA.onClick $ ToggleChatModal ShowPreview, HA.title "Preview"] "Preview",
                        HE.div (HA.class' "send-enter") [
                              HE.input [HA.type' "checkbox", HA.autocomplete "off", HA.checked messageEnter, HA.onClick ToggleMessageEnter, HA.id "message-enter"],
                              HE.label (HA.for "message-enter") "Send message on enter"
                        ]
                  ],
                  HE.div [HA.class' "chat-input-area" ] [
                        emojiButton toggleChatModal,
                        HE.textarea' [
                              HA.rows 1,
                              HA.class' "chat-input",
                              HA.id "chat-input",
                              HA.placeholder $ if isWebSocketConnected then "Type here to message " <> recipientName else "Waiting for connection...",
                              HA.disabled $ not isWebSocketConnected,
                              SK.keyDownOn "Enter" EnterBeforeSendMessage,
                              HA.onInput BeforeSendMessage,
                              HA.onInput' ResizeChatInput,
                              HA.autocomplete "off",
                              HA.value $ DM.fromMaybe "" message,
                              HA.autofocus true
                        ],
                        imageButton,
                        sendButton messageEnter
                  ]
            ]
]
      where available = DM.fromMaybe true $ getContact _.available
            recipientName = DM.fromMaybe "" $ getContact (_.name <<< _.user) <|> getProperty suggesting suggestions _.name

            getContact :: forall a. (Contact -> a) -> Maybe a
            getContact = getProperty chatting contacts

            getProperty :: forall a b. Maybe Int -> Array b -> (b -> a) -> Maybe a
            getProperty index list accessor = do
                  i <- index
                  entry <- list !! i
                  pure $ accessor entry

setJust :: forall t7 t8 t9. IsSymbol t8 => Cons t8 (Maybe t9) t7 IM => SProxy t8 -> t9 -> IMMessage
setJust field = SS.setIMField field <<< Just

bold :: Html IMMessage
bold = HE.svg [HA.class' "svg-20", HA.onClick (Apply Bold), HA.viewBox "0 0 300 300"] [
      HE.title "Bold",
      HE.path' $ HA.d "M278,205.35q0,20.73-8.74,37a77.48,77.48,0,0,1-24,27.05,114.73,114.73,0,0,1-38.9,18q-21.21,5.38-53.75,5.37H22V7.16H138.13q36.18,0,53,2.3a112.71,112.71,0,0,1,33.24,10.17q17.11,8.27,25.44,22.15t8.33,31.76q0,20.73-11.46,36.54c-7.64,10.58-18.41,18.77-32.34,24.67v1.53q29.31,5.55,46.51,23T278,205.35ZM179.14,87.93a30.65,30.65,0,0,0-3.73-14.2q-3.71-7.11-13.16-10.55c-5.62-2-12.63-3.15-21-3.37s-20.15-.27-35.28-.27H98.75V120h12.06q18.28,0,31.13-.57t20.31-3.84q10.44-4.4,13.65-11.4A38.23,38.23,0,0,0,179.14,87.93ZM197.8,204.2c0-9.07-1.84-16.09-5.51-21s-9.92-8.61-18.75-11.05c-6-1.67-14.3-2.55-24.86-2.69s-21.59-.19-33.08-.19H98.75v71.18h5.61q32.49,0,46.52-.19a70.8,70.8,0,0,0,25.87-5q12-4.8,16.56-12.77A36.38,36.38,0,0,0,197.8,204.2Z"
]

italic :: Html IMMessage
italic = HE.svg [HA.class' "svg-20", HA.onClick (Apply Italic), HA.viewBox "0 0 300 300"] [
      HE.title "Italic",
      HE.path' $ HA.d "M172.19,278H65.92l5.94-24.93H107L157,46.93H121.87L127.81,22H234.08l-5.94,24.93H193L143,253.07h35.12Z"
]

strikethrough :: Html IMMessage
strikethrough = HE.svg [HA.class' "svg-20", HA.onClick (Apply Strike), HA.viewBox "0 0 300 300"] [
      HE.title "Strikethrough",
      HE.path' (HA.d "M205.24,196.59a55.62,55.62,0,0,1-6.19,25.09,59.94,59.94,0,0,1-17.33,21,93.84,93.84,0,0,1-28.48,14.58q-16.25,5.25-39.17,5.25a204.55,204.55,0,0,1-44.23-4.37,232.6,232.6,0,0,1-39.93-13V209h2.14a127.24,127.24,0,0,0,39.86,21.15q22.62,7.43,42.45,7.43,28.13,0,43.78-10.06t15.64-26.83q0-14.44-7.39-21.29t-22.55-10.65A235.09,235.09,0,0,0,119,163.93Q105.65,162,90.7,159.11,60.45,153,45.86,138.2T31.28,99.61q0-27.27,24.15-44.7t61.23-17.42a206.72,206.72,0,0,1,44,4.37,209,209,0,0,1,35.41,10.8V86.78H194q-13-10.5-34.15-17.42a138.13,138.13,0,0,0-43.29-6.93q-24.29,0-39,9.62T62.76,96.84q0,13.57,7.31,21.3T95.89,130q9.78,2,27.81,5t30.54,6q25.35,6.41,38.17,19.39T205.24,196.59Z"),
      HE.path' (HA.d "M22,185.93v-17.5H278v17.5Z")
]

heading :: Html IMMessage
heading = HE.svg [HA.class' "svg-20", HA.onClick (Apply Heading), HA.viewBox "0 0 300 300"] [
      HE.title "Heading",
      HE.path' $ HA.d "M270.19,278H201.05V165.22H99V278H29.81V22H99v93.7h102.1V22h69.14Z"
]

unorderedList :: Html IMMessage
unorderedList = HE.svg [HA.class' "svg-other", HA.onClick (Apply UnorderedList), HA.viewBox "0 0 250 250" ] [
      HE.title "Bulleted list",
      HE.circle' [HA.cx "37.35", HA.cy "98.39", HA.r "15"],
      HE.rect' [HA.x "78.65", HA.y "91.52", HA.width "200", HA.height "8"],
      HE.circle' [HA.cx "37.35", HA.cy "150", HA.r "15"],
      HE.rect' [HA.x "78.65", HA.y "143.13", HA.width "200", HA.height "8"],
      HE.circle' [HA.cx "37.35", HA.cy "201.61", HA.r "15"],
      HE.rect' [HA.x "78.65", HA.y "194.74", HA.width "200", HA.height "8"]
]

orderedList :: Html IMMessage
orderedList = HE.svg [HA.class' "svg-other", HA.onClick (Apply OrderedList), HA.viewBox "0 0 250 250" ] [
      HE.title "Ordered list",
      HE.rect' [HA.x "76", HA.y "92", HA.width "200", HA.height "8"],
      HE.rect' [HA.x "76", HA.y "190", HA.width "200", HA.height "8"],
      HE.rect' [HA.x "76.09", HA.y "140.28", HA.width "200", HA.height "8"],
      HE.path' [HA.d "M52,110.6H30.7v-5.32h6.74V89.16H30.7v-5a25.49,25.49,0,0,0,3-.17A8.09,8.09,0,0,0,36,83.41a3.91,3.91,0,0,0,1.67-1.33,4.11,4.11,0,0,0,.65-2h7.09v25.25H52Z"],
      HE.path' [HA.d "M54,160.91H29v-5c1.91-1.33,3.83-2.73,5.75-4.22s3.47-2.77,4.62-3.85A21.39,21.39,0,0,0,43,143.63a6.68,6.68,0,0,0,1.1-3.52,3.9,3.9,0,0,0-1.41-3.22,6.27,6.27,0,0,0-4-1.14,12.28,12.28,0,0,0-4.15.78,19.22,19.22,0,0,0-4,2H29.8v-6.78a27.22,27.22,0,0,1,4.49-1.27,28.65,28.65,0,0,1,6-.63q6,0,9.16,2.4a8.07,8.07,0,0,1,3.15,6.81A10.77,10.77,0,0,1,51,144.54a22.82,22.82,0,0,1-4.62,5.36q-2,1.72-3.92,3.16c-1.32,1-2.26,1.64-2.81,2H54Z"],
      HE.path' [HA.d "M50.9,196.94a6.63,6.63,0,0,1,1.67,2,6.4,6.4,0,0,1,.62,3.06,9.09,9.09,0,0,1-.89,4,8.36,8.36,0,0,1-2.73,3.19,13.54,13.54,0,0,1-4.23,2,22.39,22.39,0,0,1-5.91.68,33.57,33.57,0,0,1-6.81-.59A27.28,27.28,0,0,1,28,209.94v-6.7h.84a21.25,21.25,0,0,0,4.4,1.85,16.19,16.19,0,0,0,4.69.77,20.36,20.36,0,0,0,2.7-.19,5.93,5.93,0,0,0,2.47-.85,4,4,0,0,0,1.26-1.24,3.8,3.8,0,0,0,.47-2.1,3,3,0,0,0-.64-2,3.52,3.52,0,0,0-1.69-1A9.22,9.22,0,0,0,40,198l-2.75,0H35.46v-5.44h1.83c1.11,0,2.1,0,3-.1a7.24,7.24,0,0,0,2.19-.48,3.26,3.26,0,0,0,1.42-1,3.21,3.21,0,0,0,.47-1.89,2.23,2.23,0,0,0-.49-1.49,3.59,3.59,0,0,0-1.25-.89,6.76,6.76,0,0,0-2-.49,19,19,0,0,0-2-.12,16.35,16.35,0,0,0-4.38.66,20.56,20.56,0,0,0-4.56,1.92h-.8V182a32.54,32.54,0,0,1,4.79-1.3,30.57,30.57,0,0,1,6.13-.62,25,25,0,0,1,5.29.5A13.58,13.58,0,0,1,48.88,182a6.9,6.9,0,0,1,2.62,2.44,6.41,6.41,0,0,1,.86,3.36,6.91,6.91,0,0,1-1.65,4.53,7.39,7.39,0,0,1-4.35,2.55v.29a10.94,10.94,0,0,1,2.31.57A7.31,7.31,0,0,1,50.9,196.94Z"]
]

linkButton :: ShowChatModal -> Html IMMessage
linkButton toggle = HE.svg [HA.class' "svg-other link-button", HA.onClick <<< ToggleChatModal $ if toggle == ShowLinkForm then HideChatModal else ShowLinkForm, HA.viewBox "0 0 300 300" ] [
      HE.title "Add link",
      HE.path' [HA.d "M127.14,181.74H44.08A22.1,22.1,0,0,1,22,159.66V140.34a22.1,22.1,0,0,1,22.08-22.08h83.06a22.12,22.12,0,0,1,22.1,22.08v19.32A22.12,22.12,0,0,1,127.14,181.74ZM44.08,123.1a17.25,17.25,0,0,0-17.23,17.24v19.32A17.25,17.25,0,0,0,44.08,176.9h83.06a17.26,17.26,0,0,0,17.25-17.24V140.34a17.26,17.26,0,0,0-17.25-17.24Z"],
      HE.path' [HA.d "M255.92,181.74H172.86a22.12,22.12,0,0,1-22.1-22.08V140.34a22.12,22.12,0,0,1,22.1-22.08h83.06A22.1,22.1,0,0,1,278,140.34v19.32A22.1,22.1,0,0,1,255.92,181.74ZM172.86,123.1a17.26,17.26,0,0,0-17.25,17.24v19.32a17.26,17.26,0,0,0,17.25,17.24h83.06a17.25,17.25,0,0,0,17.23-17.24V140.34a17.25,17.25,0,0,0-17.23-17.24Z"],
      HE.rect' [HA.x "51.2", HA.y "139.8", HA.width "199", HA.height "23", HA.rx "0.4"]
]

emojiButton :: ShowChatModal -> Html IMMessage
emojiButton toggle = HE.svg [HA.onClick <<< ToggleChatModal $ if toggle == ShowEmojis then HideChatModal else ShowEmojis, HA.class' "svg-32 emoji-access", HA.viewBox "0 0 300 300", SK.keyDownOn "Escape" $ ToggleChatModal HideChatModal] [
      HE.title "Emojis",
      HE.path' [HA.d "M150,278.5A128.5,128.5,0,1,1,278.5,150,128.64,128.64,0,0,1,150,278.5Zm0-256A127.5,127.5,0,1,0,277.5,150,127.65,127.65,0,0,0,150,22.5Z"],
      HE.ellipse' [HA.cx "97.68", HA.cy "125.87", HA.rx "10.67", HA.ry "11.43"],
      HE.ellipse' [HA.cx "201.81", HA.cy "123.84", HA.rx "10.67", HA.ry "11.43"],
      HE.path' [HA.d "M148.55,245.05H147a93,93,0,0,1-54.54-19.22c-24.23-18.93-34-54.38-34.08-54.74l-.17-.63h.66l183,.38,0,.54c-1.54,21.57-25.48,44-30.26,48.34C193,236.31,171.27,245.05,148.55,245.05Zm-89.06-73.6C60.92,176.3,70.85,207.7,93,225c28.58,22.32,77.65,30,117.83-6.08,13.55-12.15,28.43-30.84,29.89-47.13Z"]
]

imageButton :: Html IMMessage
imageButton = HE.svg [HA.onClick $ ToggleChatModal ShowSelectedImage, HA.class' "svg-32 attachment", HA.viewBox "0 0 512 512"] [
      HE.path' $ HA.d "M153.456,472A136,136,0,0,1,57.289,239.834l196.6-196.6L276.52,65.857l-196.6,196.6A104,104,0,0,0,227,409.539L434.912,201.622A72,72,0,0,0,333.088,99.8L125.171,307.716a40,40,0,1,0,56.568,56.568L361.373,184.652,384,207.279,204.367,386.911A72,72,0,1,1,102.544,285.089L310.461,77.172A104,104,0,1,1,457.539,224.249L249.622,432.166A135.1,135.1,0,0,1,153.456,472Z"
]

sendButton :: Boolean -> Html IMMessage
sendButton messageEnter = HE.svg [HA.class' { "send-button svg-50": true, hidden: messageEnter }, HA.onClick ForceBeforeSendMessage, HA.viewBox "0 0 300 300"] [
      HE.title "Send message",
      HE.path' (HA.d "M150,278.5A128.5,128.5,0,1,1,278.5,150,128.64,128.64,0,0,1,150,278.5Zm0-256A127.5,127.5,0,1,0,277.5,150,127.65,127.65,0,0,0,150,22.5Z"),
      HE.polygon' (HA.points "99.76 213.29 125.13 153.56 99.76 93.81 241.34 153.56 99.76 213.29")
]

emojiModal  :: IMModel -> Html IMMessage
emojiModal { toggleChatModal }= HE.div [HA.class' { "emoji-wrapper": true, hidden: toggleChatModal /= ShowEmojis }] <<< HE.div [HA.class' "emojis", HA.onClick' SetEmoji] $ map toEmojiCategory SIE.byCategory
      where toEmojiCategory (Tuple name pairs) = HE.div_ [
                  HE.div (HA.class' "duller") name,
                  HE.div_ $ map (HE.span_ <<< _.s) pairs
            ]