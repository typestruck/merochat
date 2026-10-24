module Client.Recover.Main where

import Prelude

import Shared.Element (ElementId(RecoverRoot))

import Client.Account as CCA
import Client.Location as CCL
import Client.Navigation as CN
import Client.Network (routes)
import Client.Network as CNE
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String (Pattern(..))
import Data.String as DS
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Aff as EA
import Effect.Class as EC
import Effect.Exception as EE
import Flame (Application, Update)
import Shared.External as SE
import Shared.Network (RequestStatus(..))
import Shared.Options.Profile (passwordMinCharacters)
import Shared.Recover.Types (RecoverMessage(..), RecoverModel)
import Shared.Recover.View as SRV
import Shared.Routes (routesSpec)

recover ∷ Effect Unit
recover = EA.launchAff_ do
      inputed ← CCA.validateEmail
      case inputed of
            Nothing → pure unit
            Just email → void <<< CCA.formRequest $ routes.recover.post { body: { email, captchaResponse: "" } }

reset ∷ String → Effect Unit
reset token = EA.launchAff_ do
      passwordInputed ← CCA.validatePassword
      passwordConfirmationInputed ← CCA.validatePasswordConfirmation passwordInputed
      case passwordConfirmationInputed of
            Just password →
                  do
                        status ← CCA.formRequest $ routes.recover.reset { body: { token, password } }
                        when (status == Success) do
                              EA.delay $ Milliseconds 3000.0
                              EC.liftEffect <<< CCL.setLocation $ routesSpec.login.get {}
            _ → pure unit

main ∷ Effect Unit
main = do
      parameter ← CCL.queryParameter "token"
      case parameter of
            Nothing → do
                  CCA.registerEvents recover
            Just token → do
                  CCA.registerEvents (reset token)

initialModel ∷ RecoverModel
initialModel = { email: Nothing, password: Nothing, confirmation: Nothing, token: Nothing, loading: false, error: Nothing, success: Nothing }

application ∷ Application RecoverModel RecoverMessage
application =
      { model: initialModel
      , view: \model → SE.view CN.navigate RecoverRoot [ SRV.viewWithNavigation CN.navigate model ]
      , update
      , subscribe: []
      }

update ∷ Update RecoverModel RecoverMessage
update model = case _ of
      SetEmail email → setEmail email model
      SetPassword password → setPassword password model
      SetConfirmation confirmation → setConfirmation confirmation model
      SetToken token → setToken token model
      Finished success → finished success model
      Failed error → failed error model
      Submit → submit model

setEmail ∷ Maybe String → RecoverModel → RecoverModel /\ Array (Aff (Maybe RecoverMessage))
setEmail email model = model { email = email, error = Nothing } /\ []

setPassword ∷ Maybe String → RecoverModel → RecoverModel /\ Array (Aff (Maybe RecoverMessage))
setPassword password model = model { password = password, error = Nothing } /\ []

setConfirmation ∷ Maybe String → RecoverModel → RecoverModel /\ Array (Aff (Maybe RecoverMessage))
setConfirmation confirmation model = model { confirmation = confirmation, error = Nothing } /\ []

setToken ∷ Maybe String → RecoverModel → RecoverModel /\ Array (Aff (Maybe RecoverMessage))
setToken token model = model { token = token, password = Nothing, confirmation = Nothing, error = Nothing, success = Nothing } /\ []

finished ∷ String → RecoverModel → RecoverModel /\ Array (Aff (Maybe RecoverMessage))
finished success model = model { loading = false, success = Just success } /\ []

failed ∷ String → RecoverModel → RecoverModel /\ Array (Aff (Maybe RecoverMessage))
failed error model = model { loading = false, error = Just error } /\ []

submit ∷ RecoverModel → RecoverModel /\ Array (Aff (Maybe RecoverMessage))
submit model =
      let
            email = DS.trim $ DM.fromMaybe "" model.email
            password = DM.fromMaybe "" model.password
            confirmation = DM.fromMaybe "" model.confirmation
      in
            case model.token of
                  Just token
                        | DS.length password < passwordMinCharacters → model { error = Just $ "Password must be " <> show passwordMinCharacters <> " characters or more" } /\ []
                        | password /= confirmation → model { error = Just "Password and confirmation do not match" } /\ []
                        | otherwise → runRequest model do
                                void $ CNE.silentRequest $ routes.recover.reset { body: { token, password } }
                                pure "Password changed. You can now log in."
                  Nothing
                        | not (DS.contains (Pattern "@") email) → model { error = Just "Please enter a valid email" } /\ []
                        | otherwise → runRequest model do
                                void $ CNE.silentRequest $ routes.recover.post { body: { email, captchaResponse: "" } }
                                pure "Recovery email sent. Please check your inbox."

runRequest ∷ RecoverModel → Aff String → RecoverModel /\ Array (Aff (Maybe RecoverMessage))
runRequest model action
      | model.loading = model /\ []
      | otherwise = model { loading = true, error = Nothing, success = Nothing } /\
              [ EA.attempt action <#> case _ of
                      Left error → Just <<< Failed $ EE.message error
                      Right success → Just $ Finished success
              ]
