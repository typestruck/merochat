--missing tests
module Server.Recover.Action where

import Prelude
import Server.Effect

import Data.Maybe (Maybe(..))
import Data.String as DS
import Data.UUID as DU
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Renderer.String as FRS
import Run as R
import Server.AccountValidation as SA
import Server.Captcha as SC
import Server.Database.Users (By(..))
import Server.Database.Users as SDU
import Server.Email as SE
import Server.Recover.Database as SRD
import Server.Response as SR
import Server.Token as ST
import Shared.Account (ResetPassword, RecoverAccount)
import Shared.Options.Domain (domain)
import Shared.Routes (routes)

accountNotFound ∷ String
accountNotFound = "Could not find an account with this email"

invalidRecovery ∷ String
invalidRecovery = "Invalid recovery link"

recover ∷ RecoverAccount → ServerEffect Unit
recover { email: rawEmail, captchaResponse } = do
      SC.validateCaptcha captchaResponse
      email ← SA.validateEmail rawEmail
      user ← SDU.userBy $ Email email
      case user of
            Nothing → SR.throwBadRequest accountNotFound
            Just { id } → do
                  token ← R.liftEffect (DU.toString <$> DU.genUUID)
                  SRD.insertRecover id token
                  contents ← R.liftEffect <<< FRS.render $ HE.html_
                        [ HE.head_ $ HE.title "MeroChat password recovery"
                        , HE.body_
                                [ HE.text "Hello!"
                                , HE.br
                                , HE.br
                                , HE.a (HA.href $ DS.joinWith "" [ "https://", domain, routes.recover.get { query: { token: Just token } } ]) "Click here to reset your password."
                                , HE.text " If you didn't ask to recover your password, just ignore this email. Your account will be safe."
                                ]
                        ]
                  SE.sendEmail email "Reset password" contents

reset ∷ ResetPassword → ServerEffect Unit
reset { token, password } = do
      void $ SA.validatePassword password
      maybeId ← SRD.selectRecoverer token
      case maybeId of
            Nothing → SR.throwBadRequest invalidRecovery
            Just id → do
                  hashedPassword ← ST.hashPassword password
                  SRD.recoverPassword token id hashedPassword
