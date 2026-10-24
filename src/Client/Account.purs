module Client.Account where

import Prelude

import Client.Ui.Document as CUD
import Client.Ui.Element as CUE
import Client.Ui.Event as CUEV
import Client.Network as CNN
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as DS
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as EA
import Effect.Class as EC
import Payload.Client (ClientResponse)
import Shared.Account (EmailPassword)
import Shared.Element (ElementId(..))
import Shared.Network (RequestStatus)
import Shared.Options.Profile (passwordMinCharacters)
import Shared.Unsafe as SU
import Web.DOM.Element (Element)
import Web.DOM.Element as WDE
import Web.HTML.Event.EventTypes (change, click)
import Web.UIEvent.KeyboardEvent as WUK
import Web.UIEvent.KeyboardEvent.EventTypes (keyup)

formSelector ∷ String
formSelector = ".form-up"

buttonSelector ∷ String
buttonSelector = formSelector <> " input[type=button], " <> formSelector <> " button[type=button]:not(#" <> show TemporaryUserSignUp <> ")"

validateEmailPassword ∷ Aff (Maybe EmailPassword)
validateEmailPassword = do
      maybeEmail ← validateEmail
      maybePassword ← validatePassword

      case maybeEmail, maybePassword of
            (Just email), (Just password) →
                  pure $ Just
                        { email: email
                        , password: password
                        }
            _, _ → pure Nothing

validateEmail ∷ Aff (Maybe String)
validateEmail = do
      emailElement ← EC.liftEffect $ CUD.unsafeGetElementById EmailInput
      email ← CUE.value emailElement
      EC.liftEffect $ WDE.setClassName "input" emailElement

      if DS.null email || not (DS.contains (Pattern "@") email) || not (DS.contains (Pattern ".") email) then do
            EC.liftEffect $ WDE.setClassName "input error" emailElement
            pure Nothing
      else
            pure $ Just email

validatePassword ∷ Aff (Maybe String)
validatePassword = do
      passwordElement ← EC.liftEffect $ CUD.unsafeGetElementById PasswordInput
      password ← CUE.value passwordElement
      EC.liftEffect $ WDE.setClassName "input" passwordElement

      if DS.length password < passwordMinCharacters then do
            EC.liftEffect $ WDE.setClassName "input error" passwordElement
            pure Nothing
      else
            pure $ Just password

listenValidatePasswordConfirmation ∷ Aff (Maybe String)
listenValidatePasswordConfirmation = do
      passwordElement ← EC.liftEffect $ CUD.unsafeGetElementById PasswordInput
      password ← CUE.value passwordElement
      validatePasswordConfirmation $ Just password

validatePasswordConfirmation ∷ Maybe String → Aff (Maybe String)
validatePasswordConfirmation password = do
      passwordConfirmationElement ← EC.liftEffect $ CUD.unsafeGetElementById PasswordConfirmationInput
      passwordConfirmation ← CUE.value passwordConfirmationElement
      EC.liftEffect $ WDE.setClassName "input" passwordConfirmationElement

      if Just passwordConfirmation /= password then do
            EC.liftEffect $ WDE.setClassName "input error" passwordConfirmationElement
            pure Nothing
      else
            pure password

registerEvents ∷ Effect Unit → Effect Unit
registerEvents callback = do
      formDiv ← CUD.unsafeQuerySelector formSelector
      button ← CUD.unsafeQuerySelector buttonSelector
      emailElement ← CUD.getElementById EmailInput
      passwordElement ← CUD.getElementById PasswordInput
      confirmPasswordElement ← CUD.getElementById PasswordConfirmationInput
      listenIfExists emailElement validateEmail
      listenIfExists passwordElement validatePassword
      listenIfExists confirmPasswordElement listenValidatePasswordConfirmation
      CUEV.addEventListener formDiv keyup onEnter
      CUEV.addEventListener button click (const callback)
      where
      onEnter event = do
            let pressed = WUK.key <<< SU.fromJust $ WUK.fromEvent event
            when (pressed == "Enter") callback

      listenIfExists ∷ ∀ a. Maybe Element → Aff a → Effect Unit
      listenIfExists maybeElement handler = case maybeElement of
            Nothing → pure unit
            Just element → CUEV.addEventListener element change (const $ EA.launchAff_ $ void handler)

formRequest ∷ ∀ a. Aff (ClientResponse a) → Aff RequestStatus
formRequest = CNN.formRequest formSelector