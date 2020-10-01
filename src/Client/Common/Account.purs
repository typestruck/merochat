module Client.Common.Account where

import Prelude

import Client.Common.DOM as CCD
import Client.Common.Network as CCN
import Client.Common.Types (RequestStatus(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Newtype as DN
import Data.String (Pattern(..))
import Data.String as DS
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as CMEC
import Effect.Class (liftEffect)
import Payload.Client (ClientResponse)
import Shared.Options.Profile (passwordMinCharacters)
import Shared.Types (RegisterLogin)
import Shared.Unsafe as SU
import Web.DOM.Element (Element)
import Web.DOM.Element as WDE
import Web.HTML.Event.EventTypes (change, click)
import Web.UIEvent.KeyboardEvent as WUK
import Web.UIEvent.KeyboardEvent.EventTypes (keyup)

formSelector :: String
formSelector = ".form-up"

buttonSelector :: String
buttonSelector = formSelector <> " input[type=button]"

validateEmailPassword :: Effect (Maybe RegisterLogin)
validateEmailPassword = do
      maybeEmail <- validateEmail
      maybePassword <- validatePassword

      case Tuple maybeEmail maybePassword of
            Tuple (Just email) (Just password) ->
                  pure $ Just {
                        email: email,
                        password: password,
                        captchaResponse: Nothing
                  }
            _ -> pure Nothing

validateEmail :: Effect (Maybe String)
validateEmail = do
      maybeEmailDiv <- CCD.querySelector "#email-input"
      case maybeEmailDiv of
            Nothing -> pure Nothing
            Just emailDiv -> do
                  emailElement <- CCD.unsafeQuerySelector "#email"
                  email <- CCD.value emailElement
                  WDE.setClassName "input" emailDiv

                  if DS.null email || not (DS.contains (Pattern "@") email) || not (DS.contains (Pattern ".") email) then do
                        WDE.setClassName "input error" emailDiv
                        pure Nothing
                   else
                        pure $ Just email

validatePassword :: Effect (Maybe String)
validatePassword = do
      maybePasswordDiv <- CCD.querySelector "#password-input"
      case maybePasswordDiv of
            Nothing -> pure Nothing
            Just passwordDiv -> do
                  passwordElement <- CCD.unsafeQuerySelector "#password"
                  password <- CCD.value passwordElement
                  WDE.setClassName "input" passwordDiv

                  if DS.length password < passwordMinCharacters then do
                        WDE.setClassName "input error" passwordDiv
                        pure Nothing
                   else
                        pure $ Just password

validateConfirmPassword :: Effect Unit
validateConfirmPassword = do
      maybeConfirmPasswordDiv <- CCD.querySelector "#confirm-password-input"
      case maybeConfirmPasswordDiv of
            Nothing -> pure unit
            Just confirmPasswordDiv -> do
                  confirmPasswordElement <- CCD.unsafeQuerySelector "#confirm-password-input"
                  password <- CCD.value confirmPasswordElement
                  WDE.setClassName "input" confirmPasswordDiv

                  if DS.length password < passwordMinCharacters then do
                        WDE.setClassName "input error" confirmPasswordElement
                   else
                        pure unit

registerEvents :: Effect Unit -> Effect Unit
registerEvents callback = do
      formDiv <- CCD.unsafeQuerySelector formSelector
      button <- CCD.unsafeQuerySelector buttonSelector
      emailElement <- CCD.querySelector "#email"
      passwordElement <- CCD.querySelector "#password"
      confirmPasswordElement <- CCD.querySelector "#confirm-password"
      listenIfExists emailElement validateEmail
      listenIfExists passwordElement validatePassword
      listenIfExists confirmPasswordElement validateConfirmPassword
      CCD.addEventListener formDiv keyup onEnter
      CCD.addEventListener button click (const callback)
      where onEnter event = do
                  let pressed = WUK.key <<< SU.fromJust $ WUK.fromEvent event
                  when (pressed == "Enter") callback
            listenIfExists :: forall a. Maybe Element -> Effect a -> Effect Unit
            listenIfExists maybeElement handler = case maybeElement of
                  Nothing -> pure unit
                  Just element -> CCD.addEventListener element change (const handler)

formRequest :: forall a. String -> Aff (ClientResponse a) -> Aff RequestStatus
formRequest loadingMessage aff = do
      previousLabel <- liftEffect do
            button <- CCD.unsafeQuerySelector buttonSelector
            CCD.value button
      setLoading loadingMessage ""
      result <- aff
      case result of
            Right _ -> do
                  setLoading previousLabel ""
                  pure Success
            Left err -> do
                  setLoading previousLabel $ CCN.errorMessage err
                  notifySuccess
                  pure Fail
      where setLoading buttonText errorText = liftEffect do
                  button <- liftEffect $ CCD.unsafeQuerySelector buttonSelector
                  errorElement <- liftEffect $ CCD.unsafeQuerySelector "#request-error-message"
                  CCD.toggleDisabled button
                  CCD.setValue button buttonText
                  CCD.setInnerHTML errorElement errorText

            notifySuccess = liftEffect do
                  formDiv <- CCD.unsafeQuerySelector formSelector
                  WDE.setClassName "input success" formDiv