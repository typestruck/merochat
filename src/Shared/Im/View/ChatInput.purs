module Shared.Im.View.ChatInput
      ( chat
      , chatBarInput
      , imageButtonElements
      ) where

import Prelude
import Shared.Availability
import Shared.Im.Types

import Client.Common.Dom as CCD
import Client.Common.Privilege as CCP
import Client.Im.Swipe as CIT
import Data.Array ((:))
import Data.Array as DA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Symbol as TDS
import Data.Tuple (Tuple(..))
import Data.Tuple as DT
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Types (NodeData)
import Shared.Element (ElementId(..))
import Shared.Im.Contact as SIC
import Shared.Im.Emoji as SIE
import Shared.Im.Svg as SIS
import Shared.Keydown as SK
import Shared.Modal.Types (ChatModal(..), Modal(..), ScreenModal(..))
import Shared.Privilege (Privilege(..))
import Shared.Privilege as SP
import Shared.Resource (maxImageSizeKB)
import Shared.Setter as SS
import Type.Proxy (Proxy(..))
import Web.Event.Event (Event)

chat ∷ ImModel → Html ImMessage
chat model =
      HE.div [ HA.class' { "send-box": true, hidden: DM.isNothing model.chatting } ]
            [ imageModal model
            , audioModal model
            , chatBarInput (Right $ DM.fromMaybe 0 model.chatting) ChatInput model --the element should be present even if not chatting
            ]

imageModal ∷ ImModel → Html ImMessage
imageModal model =
      HE.div [ HA.class' { "image-form modal-form": true, hidden: DM.isNothing model.selectedImage } ]
            if SP.hasPrivilege SendImages model.user then
                  [ SIS.closeX [ HA.class' "cancel", HA.onClick <<< SpecialRequest $ ToggleModal HideModal ]
                  , HE.div [ HA.class' { "upload-div": true, hidden: not imageValidationFailed } ]
                          [ HE.input [ HA.id $ show ImageFileInput, HA.type' "file", HA.value "", HA.accept ".png, .jpg, .jpeg, .tif, .tiff, .bmp" ]
                          , HE.div [ HA.class' "error-message" ] [ HE.text $ "Image is larger than the " <> maxImageSizeKB <> " limit. Please select a different file." ]
                          ]
                  , HE.div [ HA.class' { "image-form-image": true, hidden: imageValidationFailed } ]
                          [ HE.img [ HA.src $ DM.maybe "" _.base64 model.selectedImage ]
                          ]
                  , HE.div [ HA.class' "image-form-controls" ]
                          [ HE.input [ HA.placeholder "Caption", HA.value $ DM.fromMaybe "" model.imageCaption, HA.id $ show ImageFormCaption, HA.type' "text", HA.onInput (SS.setJust (Proxy ∷ Proxy "imageCaption")) ]
                          , HE.svg [ HA.class' "svg-50 send-image-button", HA.onClick $ ForceSendMessage ChatInput, HA.viewBox "0 0 16 16" ] $ sendButtonElements "Send file"
                          ]
                  ]
            else
                  [ HE.input [ HA.id $ show ImageFileInput, HA.type' "file", HA.value "", HA.accept ".png, .jpg, .jpeg, .tif, .tiff, .bmp", HA.class' "hidden" ]
                  , CCP.notEnoughKarma "send images" (SpecialRequest <<< ToggleModal $ Screen ShowKarmaPrivileges)
                  , HE.div [ HA.class' "image-buttons" ] [ HE.button [ HA.class' "green-button", HA.onClick <<< SpecialRequest $ ToggleModal HideModal ] [ HE.text "Dismiss" ] ]
                  ]
      where
      imageValidationFailed = DA.elem (TDS.reflectSymbol (Proxy ∷ Proxy "selectedImage")) model.erroredFields

audioModal ∷ ImModel → Html ImMessage
audioModal model =
      HE.div [ HA.class' { "modal-form audio-form": true, hidden: model.modal /= Chat ShowAudioPrompt } ]
            if SP.hasPrivilege SendAudios model.user then
                  [ HE.div [ HA.class' "duller" ] [ HE.text "Hold the button to record. Release to send, or slide left to discard" ]
                  , audioButton [ CIT.onTouchStart $ Just BeforeAudioMessage, CIT.onTouchEnd AudioMessage ]
                  ]
            else
                  [ CCP.notEnoughKarma "send audios" (SpecialRequest <<< ToggleModal $ Screen ShowKarmaPrivileges)
                  , HE.div [ HA.class' "image-buttons" ] [ HE.button [ HA.class' "green-button", HA.onClick <<< SpecialRequest $ ToggleModal HideModal ] [ HE.text "Dismiss" ] ]
                  ]

chatBarInput ∷ Either Int Int → ElementId → ImModel → Html ImMessage
chatBarInput eid elementId model = HE.fragment
      [ emojiModal elementId model
      , HE.div [ HA.class' { hidden: not available || DM.isNothing model.chatting && DA.null model.suggestions } ]
              [ HE.div [ HA.class' "chat-input-options" ]
                      [ HE.div [ HA.class' "send-enter" ]
                              [ HE.input [ HA.type' "checkbox", HA.autocomplete "off", HA.checked model.messageEnter, HA.onClick ToggleMessageEnter, HA.id "message-enter" ]
                              , HE.label [ HA.for "message-enter" ] [ HE.text "Send message on enter" ]
                              ]
                      ]
              , HE.div [ HA.class' "chat-input-area" ]
                      [ emojiButton model
                      , HE.textarea' {- $
                        ( if elementId == ChatInput then [ HA.onKeydown (SetTyping <<< DT.snd) ] else [])
                              <> -}
                              ( filterEnterKeydown <>
                                      [ HA.class' { "chat-input": true, "editing-message": DM.isJust model.editing }
                                      , HA.id $ show elementId
                                      , HA.placeholder ("Type here to message " <> recipientName)
                                      , HA.onInput' ResizeChatInput
                                      , HA.autocomplete "off"
                                      ]
                              )
                      , HE.div [ HA.class' "chat-right-buttons" ]
                              ( [ imageButton
                                , audioButton [ HA.onClick <<< SpecialRequest <<< ToggleModal $ Chat ShowAudioPrompt ]
                                ] <> sendButton elementId model
                              )
                      ]
              ]
      ]
      where
      chatting = SIC.maybeFindContact model.chatting model.contacts
      available = DM.maybe true ((_ /= Unavailable) <<< _.availability <<< _.user) chatting
      recipientName = DM.fromMaybe "" $ case eid of
            Left suggestionId → map _.name $ DA.find ((suggestionId == _) <<< _.id) model.suggestions
            _ → map (_.name <<< _.user) chatting

      filterEnterKeydown
            | model.messageEnter = [ SK.keyDownOn "Enter" (EnterSendMessage elementId) ]
            | otherwise = []

emojiButton ∷ ImModel → Html ImMessage
emojiButton model
      | model.smallScreen = HE.div' [ HA.class' "emoji-access-div hidden" ]
      | model.modal == Chat ShowEmojis =
              HE.div [ HA.class' "emoji-access-div" ] [ SIS.closeX [ HA.onClick <<< SpecialRequest $ ToggleModal HideModal, HA.class' "emoji-access" ] ]
      | otherwise =
              HE.div [ HA.class' "emoji-access-div" ]
                    [ HE.svg [ HA.onClick <<< SpecialRequest $ ToggleModal $ Chat ShowEmojis, HA.class' "emoji-access", HA.viewBox "0 0 1024 1024" ]
                            [ HE.path' [ HA.d "M510.944 960c-247.04 0-448-200.96-448-448s200.992-448 448-448 448 200.96 448 448-200.96 448-448 448z m0-832c-211.744 0-384 172.256-384 384s172.256 384 384 384 384-172.256 384-384-172.256-384-384-384z" ]
                            , HE.path' [ HA.d "M512 773.344c-89.184 0-171.904-40.32-226.912-110.624-10.88-13.92-8.448-34.016 5.472-44.896 13.888-10.912 34.016-8.48 44.928 5.472 42.784 54.688 107.136 86.048 176.512 86.048 70.112 0 134.88-31.904 177.664-87.552 10.784-14.016 30.848-16.672 44.864-5.888 14.016 10.784 16.672 30.88 5.888 44.864C685.408 732.32 602.144 773.344 512 773.344zM368 515.2c-26.528 0-48-21.472-48-48v-64c0-26.528 21.472-48 48-48s48 21.472 48 48v64c0 26.496-21.504 48-48 48zM656 515.2c-26.496 0-48-21.472-48-48v-64c0-26.528 21.504-48 48-48s48 21.472 48 48v64c0 26.496-21.504 48-48 48z" ]
                            ]
                    ]

imageButton ∷ Html ImMessage
imageButton = HE.svg [ HA.onClick <<< SpecialRequest <<< ToggleModal $ Chat ShowSelectedImage, HA.class' "attachment-button", HA.viewBox "0 0 16 16" ]
      $ imageButtonElements "Send image"

imageButtonElements ∷ String → Array (Html ImMessage)
imageButtonElements title =
      [ HE.title [ HE.text title ]
      , HE.path' [ HA.class' "strokeless", HA.d "M10.91,4v8.78a2.44,2.44,0,0,1-.72,1.65A3.31,3.31,0,0,1,8,15.25H7.67a2.67,2.67,0,0,1-2.58-2.48L5.26,2.9V2.82l0-.2h0a2,2,0,0,1,.19-.7v0a1.82,1.82,0,0,1,1.6-1A1.69,1.69,0,0,1,7.73,1,2.14,2.14,0,0,1,9.16,2.81h0v7.81c0,.75-.36,1.26-1.13,1.26A1.12,1.12,0,0,1,6.9,10.63V4H6.11v6.61a1.93,1.93,0,0,0,2,2,1.83,1.83,0,0,0,1.82-2l0-7.81c0-.06,0-.12,0-.18s0-.11,0-.17,0,0,0-.05a2.59,2.59,0,0,0-.32-1s0,0,0,0A3.19,3.19,0,0,0,7.77.09h0A2.41,2.41,0,0,0,7.09,0a2.56,2.56,0,0,0-1,.21H6A2.74,2.74,0,0,0,4.76,1.39h0a3,3,0,0,0-.37,1.43v10A3.41,3.41,0,0,0,7.67,16H8A4,4,0,0,0,10.69,15a3.22,3.22,0,0,0,.93-2.18V4Z" ]
      ]

audioButton ∷ Array (NodeData ImMessage) → Html ImMessage
audioButton actions = HE.svg (actions <> [ HA.class' "attachment-button audio-button", HA.viewBox "0 0 512 512" ])
      [ HE.g_
              [ HE.path' [ HA.class' "strokeless", HA.d "m439.5,236c0-11.3-9.1-20.4-20.4-20.4s-20.4,9.1-20.4,20.4c0,70-64,126.9-142.7,126.9-78.7,0-142.7-56.9-142.7-126.9 0-11.3-9.1-20.4-20.4-20.4s-20.4,9.1-20.4,20.4c0,86.2 71.5,157.4 163.1,166.7v57.5h-23.6c-11.3,0-20.4,9.1-20.4,20.4 0,11.3 9.1,20.4 20.4,20.4h88c11.3,0 20.4-9.1 20.4-20.4 0-11.3-9.1-20.4-20.4-20.4h-23.6v-57.5c91.6-9.3 163.1-80.5 163.1-166.7z" ]
              , HE.path' [ HA.class' "strokeless", HA.d "m256,323.5c51,0 92.3-41.3 92.3-92.3v-127.9c0-51-41.3-92.3-92.3-92.3s-92.3,41.3-92.3,92.3v127.9c0,51 41.3,92.3 92.3,92.3zm-52.3-220.2c0-28.8 23.5-52.3 52.3-52.3s52.3,23.5 52.3,52.3v127.9c0,28.8-23.5,52.3-52.3,52.3s-52.3-23.5-52.3-52.3v-127.9z" ]
              ]
      ]

sendButton ∷ ElementId → ImModel → Array (Html ImMessage)
sendButton elementId model
      | model.messageEnter = []
      | otherwise =
              [ HE.div
                      [ HA.class' "send-button-div", HA.onClick $ ForceSendMessage elementId ]
                      [ HE.svg [ HA.class' "send-button", HA.viewBox "0 0 16 16" ] $ sendButtonElements "Send message" ]
              ]

sendButtonElements ∷ String → Array (Html ImMessage)
sendButtonElements title =
      [ HE.title [ HE.text title ]
      , HE.path' [ HA.class' "strokeless", HA.d "M8,0a8,8,0,1,0,8,8A8,8,0,0,0,8,0ZM8,15.25A7.25,7.25,0,1,1,15.25,8,7.26,7.26,0,0,1,8,15.25Z" ]
      , HE.polygon' [ HA.class' "strokeless", HA.points "4.02 4.02 6.06 7.99 4.02 12.01 13.63 8 4.02 4.02" ]
      ]

emojiModal ∷ ElementId → ImModel → Html ImMessage
emojiModal elementId model
      | model.smallScreen || model.modal /= Chat ShowEmojis = HE.div' [ HA.class' "emoji-wrapper hidden" ]
      | otherwise =
              HE.div [ HA.class' "emoji-wrapper" ]
                    [ HE.div [ HA.class' "emojis", emojiClickEvent (SetEmoji elementId) ] $ map toEmojiCategory SIE.byCategory
                    ]
              where
              toEmojiCategory (Tuple name pairs) = HE.div_
                    [ HE.div [ HA.class' "duller" ] [ HE.text name ]
                    , HE.div_ $ map (\c → HE.span_ [ HE.text c.s ]) pairs
                    ]

emojiClickEvent ∷ (Event → ImMessage) → NodeData ImMessage
emojiClickEvent message = HA.createRawEvent "click" handler
      where
      handler event
            | CCD.tagNameFromTarget event == "SPAN" = pure <<< Just $ message event
            | otherwise = pure Nothing
