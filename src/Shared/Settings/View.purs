module Shared.Settings.View (view, formId) where

import Prelude

import Client.Dom as CCD
import Data.Array ((:))
import Data.Array as DA
import Data.Enum as DE
import Data.Int as DI
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String (Pattern(..))
import Data.String as DS
import Data.String.CodePoints as DSC
import Data.Symbol (class IsSymbol)
import Flame (Html)
import Flame.Html.Attribute (ToSpecialEvent)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Prim.Row (class Cons)
import Prim.Symbol (class Append)
import Record as R
import Shared.Element (ElementId(..))
import Shared.Options.Profile (emailMaxCharacters, passwordMaxCharacters, passwordMinCharacters)
import Shared.Resource (maxImageSizeKB)
import Shared.Settings.Types (SM, SettingsMessage(..), SettingsModel, Tab(..))
import Shared.Unsafe as SU
import Shared.User (ProfileVisibility(..))
import Type.Data.Symbol as TDS
import Type.Proxy (Proxy(..))
import Web.DOM.Element as WDE
import Web.Event.Event as WEE

view ∷ SettingsModel → Html SettingsMessage
view model =
      HE.div [ HA.id "settings-edition", HA.class' { hidden: not model.visible } ]
            [ HE.div [ HA.class' "modal-section" ]
                    [ HE.div [ HA.class' "green-tab" ]
                            [ HE.div [ HA.onClick $ setTab Privacy, HA.class' { "regular-green-tab": true, "selected-green-tab": model.tab == Privacy } ] [ HE.text "Privacy" ]
                            , HE.div [ HA.onClick $ setTab Account, HA.class' { "regular-green-tab": true, "selected-green-tab": model.tab == Account } ] [ HE.text "Account" ]
                            , HE.div [ HA.onClick $ setTab Chats, HA.class' { "regular-green-tab": true, "selected-green-tab": model.tab == Chats } ] [ HE.text "Chats" ]
                            ]
                    , HE.div [ HA.class' "modal-part" ]
                            [ case model.tab of
                                    Privacy → privacySection model
                                    Account → accountSection model
                                    Chats → chatsSection model
                            ]
                    ]
            ]
      where
      setTab t = SetSField $ _ { tab = t }

privacySection ∷ SettingsModel → Html SettingsMessage
privacySection model = HE.div [ HA.id $ show PrivacySettings ]
      [ HE.label_ [ HE.text "Profile visibility" ]
      , HE.select [ HA.class' "modal-input", HA.onInput (\v → SetSField (_ { profileVisibility = SU.fromJust (DE.toEnum =<< DI.fromString v) })) ]
              [ HE.option [ HA.selected $ model.profileVisibility == Everyone, HA.value <<< show $ DE.fromEnum Everyone ] [ HE.text "Show profile (default)" ]
              , HE.option [ HA.selected $ model.profileVisibility == NoTemporaryUsers, HA.value <<< show $ DE.fromEnum NoTemporaryUsers ] [ HE.text "Show profile only to registered users" ]
              , HE.option [ HA.selected $ model.profileVisibility == Contacts, HA.value <<< show $ DE.fromEnum Contacts ] [ HE.text "Show profile only to contacts" ]
              , HE.option [ HA.selected $ model.profileVisibility == Nobody, HA.value <<< show $ DE.fromEnum Nobody ] [ HE.text "Do not model.show profile" ]
              ]

      , HE.div [ HA.class' { duller: true, hidden: model.profileVisibility /= Everyone } ] [ HE.text "All users can see your profile and send you messages" ]
      , HE.div [ HA.class' { duller: true, hidden: model.profileVisibility /= NoTemporaryUsers } ]
              [ HE.text "All users (excluding quick sign up users) can see your"
              , HE.br
              , HE.text "profile and send you messages"
              ]
      , HE.div [ HA.class' { duller: true, hidden: model.profileVisibility /= Contacts } ]
              [ HE.text "Only users you have previously messaged can see your"
              , HE.br
              , HE.text "profile or send you messages"
              ]
      , HE.div [ HA.class' { duller: true, hidden: model.profileVisibility /= Nobody } ] [ HE.text "No one can see your profile or message you" ]

      , HE.label_ [ HE.text "Posts visibility" ]
      , HE.select [ HA.class' "modal-input", HA.onInput (\v → SetSField (_ { postsVisibility = SU.fromJust (DE.toEnum =<< DI.fromString v) })) ]
              [ HE.option [ HA.selected $ model.postsVisibility == Everyone, HA.value <<< show $ DE.fromEnum Everyone ] [ HE.text "Show posts (default)" ]
              , HE.option [ HA.selected $ model.postsVisibility == NoTemporaryUsers, HA.value <<< show $ DE.fromEnum NoTemporaryUsers ] [ HE.text "Show posts only to registered users" ]
              , HE.option [ HA.selected $ model.postsVisibility == Contacts, HA.value <<< show $ DE.fromEnum Contacts ] [ HE.text "Show posts only to contacts" ]
              , HE.option [ HA.selected $ model.postsVisibility == Nobody, HA.value <<< show $ DE.fromEnum Nobody ] [ HE.text "Do not model.show posts" ]
              ]
      , HE.div [ HA.class' "duller" ] $ case model.postsVisibility of
              Everyone → [ HE.text "All users can see your posts" ]
              NoTemporaryUsers → [ HE.text "All users (excluding quick sign up users) can see your posts" ]
              Contacts → [ HE.text "Only users you have previously messaged can see your posts" ]
              Nobody → [ HE.text "No one can see your profile or message you" ]
              TemporarilyBanned → []

      , HE.label_ [ HE.text "Show:" ]
      , HE.div_
              [ HE.input [ HA.id "read-toggle", HA.type' "checkbox", HA.class' "modal-input-checkbox", HA.checked model.readReceipts, HA.onChange (SetSField (_ { readReceipts = not model.readReceipts })) ]
              , HE.label [ HA.for "read-toggle", HA.class' "inline" ] [ HE.text "Read receipts" ]
              ]
      , HE.div_
              [ HE.input [ HA.id "online-toggle", HA.type' "checkbox", HA.class' "modal-input-checkbox", HA.checked model.onlineStatus, HA.onChange (SetSField (_ { onlineStatus = not model.onlineStatus })) ]
              , HE.label [ HA.for "online-toggle", HA.class' "inline" ] [ HE.text "Online status" ]
              ]
      , HE.div_
              [ HE.input [ HA.id "message-toggle", HA.type' "checkbox", HA.class' "modal-input-checkbox", HA.checked model.messageTimestamps, HA.onChange (SetSField (_ { messageTimestamps = not model.messageTimestamps })) ]
              , HE.label [ HA.for "message-toggle", HA.class' "inline" ] [ HE.text "Message timestamps" ]
              ]
      , HE.div_
              [ HE.input [ HA.id "typing-toggle", HA.type' "checkbox", HA.class' "modal-input-checkbox", HA.checked model.typingStatus, HA.onChange (SetSField (_ { typingStatus = not model.typingStatus })) ]
              , HE.label [ HA.for "typing-toggle", HA.class' "inline" ] [ HE.text "Typing status" ]
              ]
      , HE.br
      , HE.div [ HA.class' "section-buttons privacy" ]
              [ HE.input
                      [ HA.type' "button"
                      , HA.class' "green-button"
                      , HA.value "Change privacy settings"
                      , HA.onClick ChangePrivacySettings
                      ]
              , HE.span' [ HA.class' "request-error-message" ]
              , HE.span [ HA.class' { "success-message": true, hidden: model.hideSuccessMessage } ]
                      [ HE.text "Privacy settings changed!"
                      ]
              ]
      ]

accountSection ∷ SettingsModel → Html SettingsMessage
accountSection model = HE.div_
      [ fieldConfirmationSection model (Proxy ∷ Proxy "email") "text" emailMaxCharacters validateEmail "Please enter a valid email" ChangeEmail
      , fieldConfirmationSection model (Proxy ∷ Proxy "password") "password" passwordMaxCharacters validatePassword ("Password must be " <> show passwordMinCharacters <> " characters or more") ChangePassword
      , terminate
      ]
      where
      terminate = HE.div [ HA.id (formId (Proxy ∷ Proxy "confirmTermination")) ]
            [ HE.label_ [ HE.text "Permanently delete all my data and close my account" ]
            , HE.div [ HA.class' "section-buttons margined" ]
                    [ HE.input [ HA.type' "button", HA.value "Terminate account", HA.class' "green-button danger", HA.onClick ToggleTerminateAccount ]
                    , HE.span' [ HA.class' "request-error-message" ]
                    , HE.span [ HA.class' "success-message" ]
                            [ HE.text "Account terminated!"
                            , HE.br
                            , HE.text "You will be logged out..."
                            ]
                    ]
            , HE.div [ HA.class' { "modal-placeholder-overlay": true, hidden: not model.confirmTermination } ]
                    [ HE.div [ HA.class' "confirmation" ]
                            [ HE.span [ HA.class' "bold" ] [ HE.text "Do you really want to terminate your account? All of your data will be permanently lost." ]
                            , HE.div [ HA.class' "buttons" ]
                                    [ HE.button [ HA.class' "cancel", HA.onClick ToggleTerminateAccount ] [ HE.text "Cancel " ]
                                    , HE.button [ HA.class' "green-button danger", HA.onClick TerminateAccount ] [ HE.text "Terminate" ]
                                    ]
                            ]
                    ]
            ]

chatsSection ∷ SettingsModel → Html SettingsMessage
chatsSection model = HE.div [ HA.id $ show ChatSettings ]
      [ HE.div_ [ HE.text "Chat background" ]
      , if DM.isJust model.chatBackground then
              HE.img [ HA.class' "chat-background-preview", HA.src $ DM.fromMaybe "" model.chatBackground ]
        else
              HE.div []
                    [ HE.div [ HA.class' { errored: true, hidden: not (DA.elem (TDS.reflectSymbol (Proxy ∷ _ "chatBackground")) model.erroredFields) } ]
                            [ HE.div [ HA.class' "error-message" ] [ HE.text $ "Image is larger than the " <> maxImageSizeKB <> " limit. Please select a different file" ]
                            ]
                    , HE.input
                            [ HA.onChange' BeforeSetChatBackground
                            , HA.type' "file"
                            , HA.class' "chat-background-input"
                            , HA.value ""
                            , HA.accept ".png, .jpg, .jpeg, .tif, .tiff, .bmp"
                            ]
                    ]

      , HE.div [ HA.class' "section-buttons privacy chat-background-save" ]
              [ HE.input
                      [ HA.type' "button"
                      , HA.class' "green-button"
                      , HA.disabled $ DM.isNothing model.chatBackground
                      , HA.value "Set chat background"
                      , HA.onClick SaveChatBackground
                      ]
              , HE.span' [ HA.class' "request-error-message" ]
              , HE.span [ HA.class' { "success-message": true, hidden: model.hideSuccessMessage } ]
                      [ HE.text "Chat background set!"
                      ]
              ]
      ]

fieldConfirmationSection ∷ ∀ field fieldConfirmation r t. IsSymbol field ⇒ Cons field String r SM ⇒ Append field "Confirmation" fieldConfirmation ⇒ IsSymbol fieldConfirmation ⇒ Cons fieldConfirmation String t SM ⇒ SettingsModel → Proxy field → String → Int → (String → Boolean) → String → SettingsMessage → Html SettingsMessage
fieldConfirmationSection model field inputType maxChars validator fieldErrorMessage message =
      HE.div [ HA.id $ formId field ]
            [ HE.div [ HA.class' { errored: hasErrors } ]
                    [ HE.label_ [ HE.text capitalizedStringField ]
                    , HE.input [ HA.type' inputType, HA.maxlength maxChars, HA.class' "modal-input", HA.value fieldValue, onChangeValue (setValidatedField validator field), HA.autocomplete $ "new-" <> stringField ]
                    , HE.div [ HA.class' "error-message" ] [ HE.text fieldErrorMessage ]
                    ]
            , HE.div [ HA.class' { errored: hasConfirmationErrors } ]
                    [ HE.label_ [ HE.text $ "Confirm " <> stringField ]
                    , HE.input [ HA.type' inputType, HA.maxlength maxChars, HA.autocomplete $ "new-" <> stringField, HA.class' "modal-input", HA.value fieldConfirmationValue, onChangeValue (setValidatedField (_ == fieldValue) fieldConfirmation) ]
                    , HE.div [ HA.class' "error-message" ] [ HE.text $ capitalizedStringField <> " confirmation must match " <> stringField ]
                    ]
            , HE.div [ HA.class' "section-buttons" ]
                    [ HE.input [ HA.type' "button", HA.class' "green-button", HA.disabled $ hasErrors || hasConfirmationErrors, HA.value $ "Change " <> stringField, HA.onClick messageIfValidated ]
                    , HE.span' [ HA.class' "request-error-message" ]
                    , HE.span [ HA.class' "success-message" ]
                            [ HE.text $ capitalizedStringField <> " changed!"
                            , HE.br
                            , HE.text "You will be logged out..."
                            ]
                    ]
            ]
      where
      stringField = TDS.reflectSymbol field
      capitalizedStringField = capitalize stringField
      fieldValue = R.get field model
      fieldConfirmation = TDS.append field (Proxy ∷ Proxy "Confirmation")
      stringFieldConfirmation = TDS.reflectSymbol fieldConfirmation
      fieldConfirmationValue = R.get fieldConfirmation model
      hasErrors = DA.elem stringField model.erroredFields
      hasConfirmationErrors = DA.elem stringFieldConfirmation model.erroredFields
      messageIfValidated =
            if not $ validator fieldValue then
                  setValidatedField (const false) field fieldValue
            else if fieldValue /= fieldConfirmationValue then
                  setValidatedField (const false) fieldConfirmation fieldConfirmationValue
            else
                  message

onChangeValue ∷ ∀ message. ToSpecialEvent message String
onChangeValue constructor = HA.createRawEvent "change" handler
      where
      handler event = Just <<< constructor <$> CCD.value (extractElement event)
      extractElement event = SU.fromJust do
            target ← WEE.target event
            WDE.fromEventTarget target

validateEmail ∷ String → Boolean
validateEmail email = DS.contains (Pattern "@") email && DS.contains (Pattern ".") email

validatePassword ∷ String → Boolean
validatePassword password = DS.length password >= passwordMinCharacters

setValidatedField ∷ ∀ field r. IsSymbol field ⇒ Cons field String r SM ⇒ (String → Boolean) → Proxy field → String → SettingsMessage
setValidatedField validator field value = SetSField validated
      where
      stringField = TDS.reflectSymbol field
      validated model =
            R.set field value $ model
                  { erroredFields =
                          if validator value then
                                DA.delete stringField model.erroredFields
                          else
                                stringField : model.erroredFields
                  }

capitalize ∷ String → String
capitalize str = case DS.uncons str of
      Just o → (DS.toUpper $ DSC.singleton o.head) <> o.tail
      _ → str

formId ∷ ∀ field. IsSymbol field ⇒ Proxy field → String
formId field = TDS.reflectSymbol field <> "-form"
