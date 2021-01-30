module Server.Landing.Action where

import Prelude
import Server.Types
import Shared.Types

import Server.AccountValidation as SA
import Server.ThreeK as SB
import Server.Captcha as SC
import Server.Landing.Database as SLD
import Server.Token as ST

register :: RegisterLogin -> ServerEffect String
register { captchaResponse, email: rawEmail, password } = do
      SC.validateCaptcha captchaResponse
      email <- SA.validateEmail rawEmail
      hash <- SA.validatePassword password
      SA.validateExistingEmail email

      name <- SB.generateName
      headline <- SB.generateHeadline
      description <- SB.generateDescription
      id <- SLD.createUser {
            password: hash,
            email,
            name,
            headline,
            description
      }
      ST.createToken id