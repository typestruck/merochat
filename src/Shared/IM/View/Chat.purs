module Shared.IM.View.Chat where

import Prelude
import Shared.Types

import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Tuple (Tuple(..))
import Flame (Html)
import Flame.HTML.Attribute as HA
import Flame.HTML.Element as HE
import Flame.Renderer.Hook as FRH
import Prim.Row (class Cons)
import Shared.IM.Emoji as SIE
import Shared.Markdown as SM
import Shared.Setter as SS

chat :: IMModel -> Html IMMessage
chat { chatting, suggesting, isOnline, isPreviewing, message, selectedImage, messageEnter, emojisVisible, link, linkText, linkFormVisible } =
      HE.div (HA.class' "send-box") [
            HE.input [HA.id "image-file-input", HA.type' "file", HA.class' "hidden", HA.accept ".png, .jpg, .jpeg, .tif, .tiff, .bmp"],
            HE.div (HA.class' imageFormClasses) [
                  HE.div_ [
                        HE.button [HA.onClick $ ToggleImageForm Nothing] "Close"
                  ],
                  HE.div_ [
                        HE.img <<< HA.src $ DM.fromMaybe "" selectedImage
                  ],
                  HE.div (HA.class' "send-image") [
                        HE.input [HA.placeholder "Image Caption", HA.onInput (setJust (SProxy :: SProxy "imageCaption"))],
                        HE.button [HA.class' "action-button", HA.onClick (BeforeSendMessage true $ DM.fromMaybe "" message)] "Send"
                  ]
            ],
            HE.div (HA.class' linkFormClasses) [
                  HE.div_ [
                        HE.button [HA.onClick ToggleLinkForm] "Close"
                  ],
                  HE.div_ [
                        HE.input [HA.type' "text", HA.placeholder "Text", HA.value $ DM.fromMaybe "" linkText, HA.onInput (setJust (SProxy :: SProxy "linkText"))],
                        HE.input [HA.type' "text", HA.placeholder "Link", HA.value $ DM.fromMaybe "" link, HA.onInput (setJust (SProxy :: SProxy "link"))]
                  ],
                  HE.button [HA.class' "action-button", HA.onClick InsertLink] "Insert"
            ],
            HE.div (HA.class' emojiClasses) $ map toEmojiCategory SIE.byCategory,
            HE.div (HA.class' editorClasses) [
                  HE.div [HA.class' "chat-input-options"] [
                        bold,
                        italic,
                        strikethrough,
                        heading,
                        unorderedList,
                        orderedList,
                        linkButton,
                        image,

                        HE.button [HA.onClick Preview, HA.title "Preview"] "Preview",
                        HE.input [HA.type' "checkbox", HA.checked messageEnter, HA.onClick ToggleMessageEnter, HA.id "message-enter"],
                        HE.label (HA.for "message-enter") "Send message on enter"
                  ],
                  HE.div [HA.class' "chat-input-area"] [
                        if emojisVisible then
                              HE.button [HA.class' "emoji-access", HA.onClick ToggleEmojisVisible] "Close emojis"
                         else
                              HE.svg [HA.onClick ToggleEmojisVisible, HA.class' "svg-32 emoji-access", HA.viewBox "0 0 300 300"] [
                                    HE.title "Emojis",
                                    HE.path' [HA.d "M150,278.5A128.5,128.5,0,1,1,278.5,150,128.64,128.64,0,0,1,150,278.5Zm0-256A127.5,127.5,0,1,0,277.5,150,127.65,127.65,0,0,0,150,22.5Z"],
                                    HE.ellipse' [HA.cx "97.68", HA.cy "125.87", HA.rx "10.67", HA.ry "11.43"],
                                    HE.ellipse' [HA.cx "201.81", HA.cy "123.84", HA.rx "10.67", HA.ry "11.43"],
                                    HE.path' [HA.d "M148.55,245.05H147a93,93,0,0,1-54.54-19.22c-24.23-18.93-34-54.38-34.08-54.74l-.17-.63h.66l183,.38,0,.54c-1.54,21.57-25.48,44-30.26,48.34C193,236.31,171.27,245.05,148.55,245.05Zm-89.06-73.6C60.92,176.3,70.85,207.7,93,225c28.58,22.32,77.65,30,117.83-6.08,13.55-12.15,28.43-30.84,29.89-47.13Z"]
                              ],
                        HE.textarea' [
                              HA.rows 1,
                              HA.class' "chat-input",
                              HA.id "chat-input",
                              HA.placeholder $ if isOnline then "Type a message or drag files here" else "Waiting for connection...",
                              HA.autofocus true,
                              HA.disabled $ not isOnline,
                              HA.onKeyup' SetUpMessage,
                              HA.value $ DM.fromMaybe "" message
                        ],
                        HE.svg [HA.class' sendClasses, HA.onClick <<< BeforeSendMessage true $ DM.fromMaybe "" message, HA.viewBox "0 0 300 300"] [
                              HE.title "Send message",
                              HE.path' (HA.d "M150,278.5A128.5,128.5,0,1,1,278.5,150,128.64,128.64,0,0,1,150,278.5Zm0-256A127.5,127.5,0,1,0,277.5,150,127.65,127.65,0,0,0,150,22.5Z"),
                              HE.polygon' (HA.points "99.76 213.29 125.13 153.56 99.76 93.81 241.34 153.56 99.76 213.29")
                        ]
                  ]
            ],
            HE.div (HA.class' previewClasses) [
                  HE.div [HA.class' "chat-input-options"] [
                        HE.button [HA.onClick ExitPreview, HA.title "Exit preview"] "Exit"
                  ],
                  HE.div' [HA.innerHTML (SM.toHTML $ DM.fromMaybe "" message)]
            ]
      ]
      where classes visible = if visible then "" else "hidden "
            editorClasses = classes $ (DM.isJust chatting || DM.isJust suggesting) && not isPreviewing
            previewClasses = classes isPreviewing
            imageFormClasses = classes (DM.isJust selectedImage) <> "image-form"
            sendClasses = classes (not messageEnter) <> "send-button svg-50"
            emojiClasses = classes emojisVisible <> "emojis"
            linkFormClasses = classes linkFormVisible <> "link-form"

            toEmojiSpan = HE.span_ <<< _.s
            toEmojiCategory (Tuple name pairs) = HE.div (HA.onClick' SetEmoji) [
                  HE.text name,
                  HE.div_ $ map toEmojiSpan pairs
            ]

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
unorderedList = HE.svg [HA.class' "svg-other", HA.onClick (Apply UnorderedList), HA.viewBox "0 0 200 200" ] [
      HE.title "Bulleted list",
      HE.circle' [HA.cx "37.35", HA.cy "98.39", HA.r "15"],
      HE.rect' [HA.x "78.65", HA.y "91.52", HA.width 200, HA.height 8],
      HE.circle' [HA.cx "37.35", HA.cy "150", HA.r "15"],
      HE.rect' [HA.x "78.65", HA.y "143.13", HA.width 200, HA.height 8],
      HE.circle' [HA.cx "37.35", HA.cy "201.61", HA.r "15"],
      HE.rect' [HA.x "78.65", HA.y "194.74", HA.width 200, HA.height 8]
]

orderedList :: Html IMMessage
orderedList = HE.svg [HA.class' "svg-other", HA.onClick (Apply OrderedList), HA.viewBox "0 0 200 200" ] [
      HE.title "Ordered list",
      HE.rect' [HA.x "76", HA.y "92", HA.width 200, HA.height 8],
      HE.rect' [HA.x "76", HA.y "190", HA.width 200, HA.height 8],
      HE.rect' [HA.x "76.09", HA.y "140.28", HA.width 200, HA.height 8],
      HE.path' [HA.d "M52,110.6H30.7v-5.32h6.74V89.16H30.7v-5a25.49,25.49,0,0,0,3-.17A8.09,8.09,0,0,0,36,83.41a3.91,3.91,0,0,0,1.67-1.33,4.11,4.11,0,0,0,.65-2h7.09v25.25H52Z"],
      HE.path' [HA.d "M54,160.91H29v-5c1.91-1.33,3.83-2.73,5.75-4.22s3.47-2.77,4.62-3.85A21.39,21.39,0,0,0,43,143.63a6.68,6.68,0,0,0,1.1-3.52,3.9,3.9,0,0,0-1.41-3.22,6.27,6.27,0,0,0-4-1.14,12.28,12.28,0,0,0-4.15.78,19.22,19.22,0,0,0-4,2H29.8v-6.78a27.22,27.22,0,0,1,4.49-1.27,28.65,28.65,0,0,1,6-.63q6,0,9.16,2.4a8.07,8.07,0,0,1,3.15,6.81A10.77,10.77,0,0,1,51,144.54a22.82,22.82,0,0,1-4.62,5.36q-2,1.72-3.92,3.16c-1.32,1-2.26,1.64-2.81,2H54Z"],
      HE.path' [HA.d "M50.9,196.94a6.63,6.63,0,0,1,1.67,2,6.4,6.4,0,0,1,.62,3.06,9.09,9.09,0,0,1-.89,4,8.36,8.36,0,0,1-2.73,3.19,13.54,13.54,0,0,1-4.23,2,22.39,22.39,0,0,1-5.91.68,33.57,33.57,0,0,1-6.81-.59A27.28,27.28,0,0,1,28,209.94v-6.7h.84a21.25,21.25,0,0,0,4.4,1.85,16.19,16.19,0,0,0,4.69.77,20.36,20.36,0,0,0,2.7-.19,5.93,5.93,0,0,0,2.47-.85,4,4,0,0,0,1.26-1.24,3.8,3.8,0,0,0,.47-2.1,3,3,0,0,0-.64-2,3.52,3.52,0,0,0-1.69-1A9.22,9.22,0,0,0,40,198l-2.75,0H35.46v-5.44h1.83c1.11,0,2.1,0,3-.1a7.24,7.24,0,0,0,2.19-.48,3.26,3.26,0,0,0,1.42-1,3.21,3.21,0,0,0,.47-1.89,2.23,2.23,0,0,0-.49-1.49,3.59,3.59,0,0,0-1.25-.89,6.76,6.76,0,0,0-2-.49,19,19,0,0,0-2-.12,16.35,16.35,0,0,0-4.38.66,20.56,20.56,0,0,0-4.56,1.92h-.8V182a32.54,32.54,0,0,1,4.79-1.3,30.57,30.57,0,0,1,6.13-.62,25,25,0,0,1,5.29.5A13.58,13.58,0,0,1,48.88,182a6.9,6.9,0,0,1,2.62,2.44,6.41,6.41,0,0,1,.86,3.36,6.91,6.91,0,0,1-1.65,4.53,7.39,7.39,0,0,1-4.35,2.55v.29a10.94,10.94,0,0,1,2.31.57A7.31,7.31,0,0,1,50.9,196.94Z"]
]

linkButton :: Html IMMessage
linkButton = HE.svg [HA.class' "svg-other", HA.onClick ToggleLinkForm, HA.viewBox "0 0 300 300" ] [
      HE.title "Add link",
      HE.path' [HA.d "M127.14,181.74H44.08A22.1,22.1,0,0,1,22,159.66V140.34a22.1,22.1,0,0,1,22.08-22.08h83.06a22.12,22.12,0,0,1,22.1,22.08v19.32A22.12,22.12,0,0,1,127.14,181.74ZM44.08,123.1a17.25,17.25,0,0,0-17.23,17.24v19.32A17.25,17.25,0,0,0,44.08,176.9h83.06a17.26,17.26,0,0,0,17.25-17.24V140.34a17.26,17.26,0,0,0-17.25-17.24Z"],
      HE.path' [HA.d "M255.92,181.74H172.86a22.12,22.12,0,0,1-22.1-22.08V140.34a22.12,22.12,0,0,1,22.1-22.08h83.06A22.1,22.1,0,0,1,278,140.34v19.32A22.1,22.1,0,0,1,255.92,181.74ZM172.86,123.1a17.26,17.26,0,0,0-17.25,17.24v19.32a17.26,17.26,0,0,0,17.25,17.24h83.06a17.25,17.25,0,0,0,17.23-17.24V140.34a17.25,17.25,0,0,0-17.23-17.24Z"],
      HE.rect' [HA.x "51.2", HA.y "139.8", HA.width 199, HA.height 23, HA.rx "0.4"]
]

image :: Html IMMessage
image = HE.svg [HA.class' "svg-other", HA.onClick SelectImage, HA.viewBox "0 0 300 300" ] [
      HE.title "Upload image",
      HE.path' [HA.d "M278,278H22V22H278ZM28.24,271.76H271.76V28.24H28.24Z"],
      HE.path' [HA.d "M249.34,251.46H46.41V48.54H249.34Zm-199.8-3.12H246.22V51.66H49.54Z"],
      HE.path' [HA.d "M243.32,246H53.51V56.15H243.32ZM56,243.46H240.83V58.63H56Z"],
      HE.path' [HA.d "M148.51,212.68a69.41,69.41,0,0,1-69.92-68.39l0-1.27h8.68l.08,1.18c2.24,33.92,29.44,60.51,62,60.51,33.53,0,61-27.34,62.44-62.25l0-1.19h6.68v1.24A70.13,70.13,0,0,1,148.51,212.68ZM81.12,145.51a66.93,66.93,0,0,0,67.39,64.69,67.63,67.63,0,0,0,67.42-66.44h-1.78c-2.1,35.68-30.37,63.44-64.88,63.44-33.44,0-61.49-27-64.37-61.69Z"],
      HE.path' [HA.d "M149.27,207.2c-33.81,0-62.12-27.61-64.44-62.86l-.1-1.29,1.32,0,128.19-1.78,0,1.32C212.68,178.8,184.15,207.2,149.27,207.2ZM87.41,145.49c2.86,33.31,29.79,59.22,61.86,59.22,33.12,0,60.27-26.66,62.36-60.93Z"],
      HE.path' [HA.d "M114.63,157.56c-1.68-.33-4.57,4.07-4.57,4.07s-3.91,6.93,2.18,8.15c5.47,1.09,4.38-6.76,4.38-6.76S116.05,157.84,114.63,157.56Z"],
      HE.path' [HA.d "M151.56,169.17c-1.67-.33-4.56,4.07-4.56,4.07s-3.92,6.93,2.18,8.15c5.46,1.09,4.37-6.76,4.37-6.76S153,169.45,151.56,169.17Z"],
      HE.path' [HA.d "M183.19,154.92c1.67-.32,4.57,4.07,4.57,4.07s3.91,6.93-2.19,8.15c-5.46,1.09-4.37-6.75-4.37-6.75S181.76,155.2,183.19,154.92Z"]
]
