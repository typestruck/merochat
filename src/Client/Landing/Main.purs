module Client.Landing.Main where

import Prelude

import Client.Account as CCA
import Client.AppId (imAppId)
import Client.Location as CCL
import Client.Lynx.Room as CLR
import Client.Navigation as CN
import Client.Network (routes)
import Client.Network as CNE
import Client.Ui.Document as CUD
import Client.Ui.Event as CUEV
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String (Pattern(..))
import Data.String as DS
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as EA
import Effect.Class as EC
import Effect.Exception as EE
import Flame (Application, Update)
import Flame as F
import Server.Landing.Template as SLT
import Shared.Element (ElementId(..))
import Shared.External as SE
import Shared.Landing.Types (LandingMessage(..), LandingModel)
import Shared.Landing.View as SLV
import Shared.Login.Types (LoginResponse)
import Shared.Network (RequestStatus(..))
import Shared.Options.Profile (passwordMinCharacters)
import Shared.Routes (routesSpec)
import Shared.Unsafe as SU
import Web.HTML.Event.EventTypes (click)

registerRegularUser ∷ Effect Unit
registerRegularUser = EA.launchAff_ do
      registerLogin ← CCA.validateEmailPassword
      case registerLogin of
            Nothing → pure unit
            Just rl → do
                  status ← CCA.formRequest $ routes.register { body: { email: rl.email, password: rl.password, captchaResponse: "" }, query: { boot: false } }
                  EC.liftEffect $ case status of
                        Success → CCL.setLocation $ routesSpec.im.get {}
                        Failure _ → pure unit

registerTemporaryUser ∷ Effect Unit
registerTemporaryUser = EA.launchAff_ do
      status ← CCA.formRequest $ routes.temporary { query: { boot: false } }
      EC.liftEffect $ case status of
            Success → CCL.setLocation $ routesSpec.im.get {}
            Failure _ → pure unit

registerTemporaryUserEvents ∷ Effect Unit
registerTemporaryUserEvents = do
      temporaryUserElement ← CUD.getElementById TemporaryUserSignUp
      CUEV.addEventListener (SU.fromJust temporaryUserElement) click (const registerTemporaryUser)

main ∷ Effect Unit
main = do
      CCA.registerEvents registerRegularUser
      registerTemporaryUserEvents

initialModel ∷ LandingModel
initialModel = { email: Nothing, password: Nothing, loading: false, error: Nothing }

application ∷ Application LandingModel LandingMessage
application =
      { model: initialModel
      , view: \model → SE.view CN.navigate LandingRoot <<< SLT.contentWithForm $ SLV.view model
      , update
      , subscribe: []
      }

update ∷ Update LandingModel LandingMessage
update model = case _ of
      SetEmail email → setEmail email model
      SetPassword password → setPassword password model
      Reset → reset model
      Failed error → failed error model
      Submit → submit model
      Guest → guest model

setEmail ∷ Maybe String → LandingModel → LandingModel /\ Array (Aff (Maybe LandingMessage))
setEmail email model = model { email = email, error = Nothing } /\ []

setPassword ∷ Maybe String → LandingModel → LandingModel /\ Array (Aff (Maybe LandingMessage))
setPassword password model = model { password = password, error = Nothing } /\ []

reset ∷ LandingModel → LandingModel /\ Array (Aff (Maybe LandingMessage))
reset _ = initialModel /\ []

failed ∷ String → LandingModel → LandingModel /\ Array (Aff (Maybe LandingMessage))
failed error model = model { loading = false, error = Just error } /\ []

submit ∷ LandingModel → LandingModel /\ Array (Aff (Maybe LandingMessage))
submit model =
      let
            email = DS.trim $ DM.fromMaybe "" model.email
            password = DM.fromMaybe "" model.password
      in
            if DS.null email || not (DS.contains (Pattern "@") email) || not (DS.contains (Pattern ".") email) then
                  model { error = Just "Please enter a valid email" } /\ []
            else if DS.length password < passwordMinCharacters then
                  model { error = Just $ "Password must be " <> show passwordMinCharacters <> " characters or more" } /\ []
            else runRequest model do
                  payload ← CNE.silentRequest $ routes.register { body: { email, password, captchaResponse: "" }, query: { boot: true } }
                  bootstrap payload

guest ∷ LandingModel → LandingModel /\ Array (Aff (Maybe LandingMessage))
guest model = runRequest model do
      payload ← CNE.silentRequest $ routes.temporary { query: { boot: true } }
      bootstrap payload

runRequest ∷ LandingModel → Aff Unit → LandingModel /\ Array (Aff (Maybe LandingMessage))
runRequest model action
      | model.loading = model /\ []
      | otherwise = model { loading = true, error = Nothing } /\
              [ EA.attempt action <#> case _ of
                      Left error → Just <<< Failed $ EE.message error
                      Right _ → Just Reset
              ]

bootstrap ∷ LoginResponse → Aff Unit
bootstrap payload =
      case payload.user of
            Nothing → EA.throwError $ EE.error "Server did not return the current user"
            Just user → do
                  CLR.insert payload.contacts
                  CLR.insert payload.suggestions
                  CLR.insert user
                  EC.liftEffect $ F.navigate imAppId
