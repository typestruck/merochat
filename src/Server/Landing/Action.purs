module Server.Landing.Action where

import Prelude
import Server.Effect

import Data.Maybe (Maybe(..))
import Data.String as DS
import Server.AccountValidation as SA
import Server.Captcha as SC
import Server.Landing.Database as SLD
import Server.ThreeK as SB
import Server.Token as ST
import Shared.Account (RegisterLogin)

registerRegularUser ∷ RegisterLogin → ServerEffect String
registerRegularUser { captchaResponse, email: rawEmail, password } = do
      SC.validateCaptcha captchaResponse
      email ← SA.validateEmail rawEmail
      hash ← SA.validatePassword password
      SA.validateExistingEmail email

      name ← SB.generateName
      headline ← SB.generateHeadline
      description ← SB.generateDescription
      id ← SLD.createUser
            { password: Just hash
            , email: Just email
            , name
            , headline
            , description
            , temporary : false
            }
      ST.createToken id

registerTemporaryUser ∷ Maybe String → ServerEffect String
registerTemporaryUser captchaResponse = do
      SC.validateCaptcha captchaResponse
      name ← SB.generateName
      headline ← SB.generateHeadline
      description ← SB.generateDescription
      id ← SLD.createUser
            { password: Nothing
            , email: Nothing
            , name
            , headline
            , description
            , temporary : true
            }
      ST.createToken id