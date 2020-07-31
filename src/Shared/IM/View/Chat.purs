module Shared.IM.View.Chat where

import Prelude
import Shared.IM.Types

import Data.Maybe as DM
import Data.String as DS
import Flame (Html)
import Flame.HTML.Attribute as HA
import Flame.HTML.Element as HE
import Shared.Markdown as SM

chat :: IMModel -> Html IMMessage
chat (IMModel { chatting, suggesting, isPreviewing, message }) =
      HE.div (HA.class' "send-box") [
            HE.div (HA.class' editorClasses) [
                  HE.div [HA.class' "chat-input-options"] [
                        HE.button [HA.onClick (Apply Bold), HA.title "Bold"] "B",
                        HE.button [HA.onClick (Apply Italic), HA.title "Italic"] "I",
                        HE.button [HA.onClick (Apply Strike), HA.title "Strikethrough"] "S",
                        HE.button [HA.onClick (Apply Heading), HA.title "Heading"] "H",
                        HE.button [HA.onClick (Apply OrderedList), HA.title "Ordered list"] "O",
                        HE.button [HA.onClick (Apply UnorderedList), HA.title "Unordered list"] "L",
                        HE.button [HA.onClick Preview, HA.title "Preview"] "Preview"
                  ],
                  HE.textarea' [
                        HA.rows 1,
                        HA.class' "chat-input",
                        HA.id "chat-input",
                        HA.placeholder "Type a message or drag files here",
                        HA.autofocus true,
                        HA.onKeyup' SetUpMessage,
                        HA.value $ DM.fromMaybe "" message
                  ]
            ],
            HE.div (HA.class' previewClasses) [
                  HE.div [HA.class' "chat-input-options"] [
                        HE.button [HA.onClick ExitPreview, HA.title "Exit preview"] "Exit"
                  ],
                  HE.div' [HA.innerHTML <<< SM.toHTML $ DM.fromMaybe "" message]
            ]
      ]
      where editorClasses = if DM.isNothing chatting && DM.isNothing suggesting || isPreviewing then "hidden" else ""
            previewClasses = if isPreviewing then "" else "hidden"
