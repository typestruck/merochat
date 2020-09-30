module Client.Common.Account where

import Prelude

import Client.Common.DOM as CCD
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as DS
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Shared.Options.Profile (passwordMinCharacters)
import Shared.Types (RegisterLogin)
import Shared.Unsafe as SU
import Web.DOM.Element as WDE
import Web.HTML.Event.EventTypes (change, click)
import Web.UIEvent.KeyboardEvent as WUK
import Web.UIEvent.KeyboardEvent.EventTypes (keyup)

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
      emailDiv <- CCD.querySelector "#email-input"
      emailElement <- CCD.querySelector "#email"
      email <- CCD.value emailElement
      WDE.setClassName "input" emailDiv

      if DS.null email || not (DS.contains (Pattern "@") email) || not (DS.contains (Pattern ".") email) then do
            WDE.setClassName "input error" emailDiv
            pure Nothing
       else
            pure $ Just email

validatePassword :: Effect (Maybe String)
validatePassword = do
      passwordDiv <- CCD.querySelector "#password-input"
      passwordElement <- CCD.querySelector "#password"
      password <- CCD.value passwordElement
      WDE.setClassName "input" passwordDiv

      if DS.length password < passwordMinCharacters then do
            WDE.setClassName "input error" passwordDiv
            pure Nothing
       else
            pure $ Just password

registerEvents :: Effect Unit -> Effect Unit
registerEvents callback = do
      formDiv <- CCD.querySelector ".form-up"
      button <- CCD.querySelector ".form-up input[type=button]"
      emailElement <- CCD.querySelector "#email"
      passwordElement <- CCD.querySelector "#password"
      CCD.addEventListener emailElement change (const validateEmail)
      CCD.addEventListener passwordElement change (const validatePassword)
      CCD.addEventListener formDiv keyup onEnter
      CCD.addEventListener button click (const callback)
      where onEnter event = do
                  let pressed = WUK.key <<< SU.fromJust $ WUK.fromEvent event
                  when (pressed == "Enter") callback
