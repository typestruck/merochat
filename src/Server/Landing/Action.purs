module Server.Landing.Action where

import Prelude
import Server.Types
import Shared.Types

import Affjax as A
import Affjax.RequestBody as RB
import Affjax.ResponseFormat as RF
import Affjax.StatusCode (StatusCode(..))
import Data.Argonaut.Decode as DAD
import Data.Either (Either(..))
import Data.Either as DE
import Data.FormURLEncoded as DF
import Data.HTTP.Method (Method(..))
import Server.Token as ST
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String as DS
import Data.Tuple (Tuple(..))
import Run as R
import Run.Reader as RR
import Server.Bender as SB
import Server.Captcha as SC
import Server.Landing.Database as SLD
import Server.Database.User as SDU
import Server.Response as SR

invalidUserEmailMessage :: String
invalidUserEmailMessage = "Invalid email or password"

emailAlreadyRegisteredMessage :: String
emailAlreadyRegisteredMessage = "Email already registered"

register :: RegisterLogin -> ServerEffect String
register { captchaResponse, email, password } = do
      when (DS.null email || DS.null password) $ SR.throwBadRequest invalidUserEmailMessage
      user <- SDU.userBy $ Email email
      when (DM.isJust user) $ SR.throwBadRequest emailAlreadyRegisteredMessage
      SC.validateCaptcha captchaResponse

      name <- SB.generateName
      headline <- SB.generateHeadline
      description <- SB.generateDescription
      hashedPassword <- ST.hashPassword password
      id <- SLD.createUser {
            password: hashedPassword,
            email,
            name,
            headline,
            description
      }
      ST.createToken id