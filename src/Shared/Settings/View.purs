module Shared.Settings.View (view, formId) where

import Prelude

import Data.Array ((:))
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String (Pattern(..))
import Data.String as DS
import Data.String.CodePoints as DSC
import Data.Symbol (class IsSymbol)
import Shared.Modal (Modal(..))
import Shared.Modal as SM
import Flame (Html)
import Flame.Html.Attribute (ToSpecialEvent)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Prim.Row (class Cons)
import Prim.Symbol (class Append)
import Record as R
import Shared.Avatar as SA
import Shared.Dropdown as SDRO
import Shared.Element (ElementId(..))
import Shared.File as SF
import Shared.Extra as SE
import Shared.Network (RequestStatus(..))
import Shared.Options.Profile (emailMaxCharacters, passwordMaxCharacters, passwordMinCharacters)
import Shared.Resource (maxImageSizeKB)
import Shared.Settings.Types (SM, SettingsMessage(..), SettingsModel, SettingRequest(..), SettingsRequestStatus, Tab(..))
import Shared.SuggestionsFrom (SuggestionsFrom(..))
import Shared.User (ProfileVisibility(..))
import Type.Data.Symbol as TDS
import Type.Proxy (Proxy(..))

view ∷ SettingsModel → Html SettingsMessage
view model =
      HE.div [ HA.id "settings-edition", HA.class' { hidden: not model.visible } ]
            [ SM.header "Settings" $ ToggleVisibility HideModal
            , HE.div [ HA.class' "modal-section" ]
                    [ HE.div [ HA.class' "green-tab lynx-inline" ]
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
      [ HE.label [ HA.class' "modal-label" ] [ HE.text "Profile visibility" ]
      , SDRO.choices SelectChoice "Profile visibility" model.profileVisibility (\value → SetSField $ _ { profileVisibility = value })
              [ { value: Everyone, name: "Show profile (default)" }
              , { value: NoTemporaryUsers, name: "Show profile only to registered users" }
              , { value: Contacts, name: "Show profile only to contacts" }
              , { value: Nobody, name: "Do not show profile" }
              ]

      , HE.div [ HA.class' { duller: true, hidden: model.profileVisibility /= Everyone } ] [ HE.text "All users can see your profile and send you messages" ]
      , HE.div [ HA.class' { duller: true, hidden: model.profileVisibility /= NoTemporaryUsers } ]
              [ HE.text "All users (excluding quick sign up users) can see your"
              , HE.text "\n"
              , HE.text "profile and send you messages"
              ]
      , HE.div [ HA.class' { duller: true, hidden: model.profileVisibility /= Contacts } ]
              [ HE.text "Only users you have previously messaged can see your"
              , HE.text "\n"
              , HE.text "profile or send you messages"
              ]
      , HE.div [ HA.class' { duller: true, hidden: model.profileVisibility /= Nobody } ] [ HE.text "No one can see your profile or message you" ]

      , HE.label [ HA.class' "modal-label" ] [ HE.text "Posts visibility" ]
      , SDRO.choices SelectChoice "Posts visibility" model.postsVisibility (\value → SetSField $ _ { postsVisibility = value })
              [ { value: Everyone, name: "Show posts (default)" }
              , { value: NoTemporaryUsers, name: "Show posts only to registered users" }
              , { value: Contacts, name: "Show posts only to contacts" }
              , { value: Nobody, name: "Do not show posts" }
              ]
      , HE.div [ HA.class' "duller" ] $ case model.postsVisibility of
              Everyone → [ HE.text "All users can see your posts" ]
              NoTemporaryUsers → [ HE.text "All users (excluding quick sign up users) can see your posts" ]
              Contacts → [ HE.text "Only users you have previously messaged can see your posts" ]
              Nobody → [ HE.text "No one can see your profile or message you" ]
              TemporarilyBanned → []

      , HE.label [ HA.class' "modal-label" ] [ HE.text "Asks" ]
      , SDRO.choices SelectChoice "Asks" model.asksVisibility (\value → SetSField $ _ { asksVisibility = value })
              [ { value: Everyone, name: "Allow asks from everyone" }
              , { value: NoTemporaryUsers, name: "Allow asks only from registered users (default)" }
              , { value: Contacts, name: "Allow asks only from contacts" }
              , { value: Nobody, name: "Do not allow asks" }
              ]
      , HE.div [ HA.class' "duller" ] $ case model.asksVisibility of
              Everyone → [ HE.text "All users can send you asks" ]
              NoTemporaryUsers → [ HE.text "All users (excluding quick sign up users) can send you asks" ]
              Contacts → [ HE.text "Only users you have previously messaged can send you asks" ]
              Nobody → [ HE.text "No one can send you asks" ]
              TemporarilyBanned → []

      , HE.label [ HA.class' "modal-label" ] [ HE.text "Show:" ]
      , SE.checkbox model.readReceipts (SetSField $ _ { readReceipts = not model.readReceipts }) $ "Read receipts"
      , SE.checkbox model.onlineStatus (SetSField $ _ { onlineStatus = not model.onlineStatus }) $ "Online status"
      , SE.checkbox model.messageTimestamps (SetSField $ _ { messageTimestamps = not model.messageTimestamps }) $ "Message timestamps"
      , SE.checkbox model.typingStatus (SetSField $ _ { typingStatus = not model.typingStatus }) $ "Typing status"
      , SE.checkbox model.lastMessageOnContactList (SetSField $ _ { lastMessageOnContactList = not model.lastMessageOnContactList }) $ "Last messages on contact list"
      , HE.text "\n"
      , HE.div [ HA.class' "section-buttons privacy" ]
              [ HE.button [ HA.class' "green-button", HA.onClick $ ChangePrivacySettings RequestSavePrivacy ] [ HE.text $ "Change privacy settings" ]
              , requestStatus RequestSavePrivacy "Privacy settings changed!" model.requestStatus
              ]
      ]

accountSection ∷ SettingsModel → Html SettingsMessage
accountSection model = HE.div_
      [ fieldConfirmationSection model (Proxy ∷ Proxy "email") "text" emailMaxCharacters validateEmail "Please enter a valid email" ChangeEmail
      , fieldConfirmationSection model (Proxy ∷ Proxy "password") "password" passwordMaxCharacters validatePassword ("Password must be " <> show passwordMinCharacters <> " characters or more") ChangePassword
      , terminate
      ]
      where
      terminate = HE.div [ HA.id <<< formId $ (Proxy ∷ Proxy "confirmTermination") ]
            [ HE.label [ HA.class' "modal-label" ] [ HE.text "Permanently delete all my data and close my account" ]
            , HE.div [ HA.class' "section-buttons margined" ]
                    [ HE.button [ HA.class' "green-button danger", HA.onClick ToggleTerminateAccount ] [ HE.text $ "Terminate account" ]
                    , accountStatus "confirmTermination" model
                    ]
            , HE.div [ HA.class' { "modal-placeholder-overlay": true, hidden: not model.confirmTermination } ]
                    [ HE.div [ HA.class' "confirmation" ]
                            [ HE.span [ HA.class' "bold" ] [ HE.text "Do you really want to terminate your account? All of your data will be permanently lost." ]
                            , HE.div [ HA.class' "buttons" ]
                                    [ HE.button [ HA.class' "cancel", HA.onClick ToggleTerminateAccount ] [ HE.text "Cancel " ]
                                    , HE.button [ HA.class' "green-button danger", HA.onClick TerminateAccount, HA.disabled model.accountLoading ] [ HE.text "Terminate" ]
                                    ]
                            ]
                    ]
            ]

chatsSection ∷ SettingsModel → Html SettingsMessage
chatsSection model = HE.div [ HA.id $ show ChatSettings ]
      [ HE.label [ HA.class' "modal-label" ] [ HE.text "Initial suggestions screen" ]
      , SDRO.choices SelectChoice "Initial suggestions screen" model.suggestionsFrom (\value → SetSField $ _ { suggestionsFrom = value })
              [ { value: ThisWeek, name: "New suggestions" }
              , { value: OnlineOnly, name: "Online users" }
              , { value: ContactsOnly, name: "Contacts" }
              , { value: FavoritesOnly, name: "Favorites" }
              ]
      , HE.div [ HA.class' "duller" ]
              [ HE.text $ case model.suggestionsFrom of
                      OnlineOnly → "Show only users who are online now"
                      ContactsOnly → "Show only users you have talked to before"
                      FavoritesOnly → "Show only users you have favorited"
                      _ → "Show all new user suggestions (default)"
              ]
      , HE.div [ HA.class' "section-buttons privacy" ]
              [ HE.button [ HA.class' "green-button", HA.onClick $ ChangePrivacySettings RequestSaveSuggestions ] [ HE.text $ "Save" ]
              , requestStatus RequestSaveSuggestions "Saved!" model.requestStatus
              ]

      , HE.div [] [ HE.text "Chat background" ]
      , SE.checkbox model.ownBackground (SetSField $ _ { ownBackground = not model.ownBackground }) $ if DM.isJust model.chatBackground then "Use this background instead of contacts'" else "No chat backgrounds"
      , if DM.isJust model.chatBackground then
              HE.img [ HA.class' "chat-background-preview", HA.src <<< DM.fromMaybe "" $ SA.fromAvatarPath model.chatBackground ]
        else
              HE.div []
                    [ HE.div [ HA.class' { errored: true, hidden: not <<< DA.elem (TDS.reflectSymbol (Proxy ∷ _ "chatBackground")) $ model.erroredFields } ]
                            [ HE.div [ HA.class' "error-message" ] [ HE.text $ "Image is larger than the " <> maxImageSizeKB <> " limit. Please select a different file" ]
                            ]
                    , HE.button [ HA.class' "modal-input chat-background-input", HA.onClick SelectChatBackground ] [ HE.text "Choose image" ]
                    ]

      , HE.div [ HA.class' "section-buttons privacy chat-background-save" ]
              [ if DM.isJust model.chatBackground && DM.isNothing (model.chatBackground >>= SF.fromBase64File) then
                      HE.button [ HA.class' "green-button danger", HA.onClick RemoveChatBackground ] [ HE.text $ "Remove chat background" ]
                else
                      HE.button [ HA.class' "green-button", HA.onClick SaveChatBackground ] [ HE.text $ "Set chat background" ]
              , requestStatus RequestSaveChatBackground "Chat background set!" model.requestStatus
              ]
      ]

fieldConfirmationSection ∷ ∀ field fieldConfirmation r t. IsSymbol field ⇒ Cons field String r SM ⇒ Append field "Confirmation" fieldConfirmation ⇒ IsSymbol fieldConfirmation ⇒ Cons fieldConfirmation String t SM ⇒ SettingsModel → Proxy field → String → Int → (String → Boolean) → String → SettingsMessage → Html SettingsMessage
fieldConfirmationSection model field inputType maxChars validator fieldErrorMessage message =
      HE.div [ HA.id $ formId field ]
            [ HE.div [ HA.class' { errored: hasErrors } ]
                    [ HE.label [ HA.class' "modal-label" ] [ HE.text capitalizedStringField ]
                    , HE.input [ HA.type' inputType, HA.maxlength maxChars, HA.class' "modal-input", HA.value fieldValue, onChangeValue <<< setValidatedField validator $ field, HA.autocomplete $ "new-" <> stringField ]
                    , HE.div [ HA.class' "error-message" ] [ HE.text fieldErrorMessage ]
                    ]
            , HE.div [ HA.class' { errored: hasConfirmationErrors } ]
                    [ HE.label [ HA.class' "modal-label" ] [ HE.text $ "Confirm " <> stringField ]
                    , HE.input [ HA.type' inputType, HA.maxlength maxChars, HA.autocomplete $ "new-" <> stringField, HA.class' "modal-input", HA.value fieldConfirmationValue, onChangeValue <<< setValidatedField (_ == fieldValue) $ fieldConfirmation ]
                    , HE.div [ HA.class' "error-message" ] [ HE.text $ capitalizedStringField <> " confirmation must match " <> stringField ]
                    ]
            , HE.div [ HA.class' "section-buttons" ]
                    [ HE.button [ HA.class' "green-button", HA.disabled $ model.accountLoading || hasErrors || hasConfirmationErrors, HA.onClick messageIfValidated ] [ HE.text $ "Change " <> stringField ]
                    , accountStatus stringField model
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
onChangeValue = HA.onInput

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

requestStatus ∷ SettingRequest → String → Maybe SettingsRequestStatus → Html SettingsMessage
requestStatus request successText = case _ of
      Just r | r.request == request → case r.status of
            Success → HE.div [] [ HE.text successText ]
            Failure _ → HE.div [ HA.class' "request-error-message" ] [ HE.text "Could not save. Please try again" ]
      _ → HE.div [] [ HE.text "" ]

accountStatus ∷ String → SettingsModel → Html SettingsMessage
accountStatus field model = case model.accountRequest of
      Just result | result.field == field → case result.status of
            Success → HE.div [ HA.class' "account-success" ] [ HE.text "Saved! You will be logged out..." ]
            Failure error → HE.div [ HA.class' "account-error" ] [ HE.text error ]
      _ → HE.div_ []
