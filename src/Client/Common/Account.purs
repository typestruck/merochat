module Client.Common.Account where

import Prelude

import Client.Common.Dom as CCD
import Client.Common.Network as CNN
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as DS
import Effect (Effect)
import Effect.Aff (Aff)
import Payload.Client (ClientResponse)
import Shared.Account (RegisterLogin)
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
buttonSelector = formSelector <> " input[type=button]"

validateEmailPassword ∷ Effect (Maybe RegisterLogin)
validateEmailPassword = do
      maybeEmail ← validateEmail
      maybePassword ← validatePassword

      case maybeEmail, maybePassword of
            (Just email), (Just password) →
                  pure $ Just
                        { email: email
                        , password: password
                        , captchaResponse: Nothing
                        }
            _, _ → pure Nothing

validateEmail ∷ Effect (Maybe String)
validateEmail = do
      emailElement ← CCD.unsafeGetElementById EmailInput
      email ← CCD.value emailElement
      WDE.setClassName "input" emailElement

      if DS.null email || not (DS.contains (Pattern "@") email) || not (DS.contains (Pattern ".") email) then do
            WDE.setClassName "input error" emailElement
            pure Nothing
      else
            pure $ Just email

validatePassword ∷ Effect (Maybe String)
validatePassword = do
      passwordElement ← CCD.unsafeGetElementById PasswordInput
      password ← CCD.value passwordElement
      WDE.setClassName "input" passwordElement

      if DS.length password < passwordMinCharacters then do
            WDE.setClassName "input error" passwordElement
            pure Nothing
      else
            pure $ Just password

listenValidatePasswordConfirmation ∷ Effect (Maybe String)
listenValidatePasswordConfirmation = do
      passwordElement ← CCD.unsafeGetElementById PasswordInput
      password ← CCD.value passwordElement
      validatePasswordConfirmation $ Just password

validatePasswordConfirmation ∷ Maybe String → Effect (Maybe String)
validatePasswordConfirmation password = do
      passwordConfirmationElement ← CCD.unsafeGetElementById PasswordConfirmationInput
      passwordConfirmation ← CCD.value passwordConfirmationElement
      WDE.setClassName "input" passwordConfirmationElement

      if Just passwordConfirmation /= password then do
            WDE.setClassName "input error" passwordConfirmationElement
            pure Nothing
      else
            pure password

registerEvents ∷ Effect Unit → Effect Unit
registerEvents callback = do
      formDiv ← CCD.unsafeQuerySelector formSelector
      button ← CCD.unsafeQuerySelector buttonSelector
      emailElement ← CCD.getElementById EmailInput
      passwordElement ← CCD.getElementById PasswordInput
      confirmPasswordElement ← CCD.getElementById PasswordConfirmationInput
      listenIfExists emailElement validateEmail
      listenIfExists passwordElement validatePassword
      listenIfExists confirmPasswordElement listenValidatePasswordConfirmation
      CCD.addEventListener formDiv keyup onEnter
      CCD.addEventListener button click (const callback)
      where
      onEnter event = do
            let pressed = WUK.key <<< SU.fromJust $ WUK.fromEvent event
            when (pressed == "Enter") callback

      listenIfExists ∷ ∀ a. Maybe Element → Effect a → Effect Unit
      listenIfExists maybeElement handler = case maybeElement of
            Nothing → pure unit
            Just element → CCD.addEventListener element change (const handler)

formRequest ∷ ∀ a. Aff (ClientResponse a) → Aff RequestStatus
formRequest = CNN.formRequest formSelector