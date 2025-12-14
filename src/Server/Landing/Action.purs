module Server.Landing.Action where

import Prelude
import Server.Effect

import Data.Maybe (Maybe(..))
import Server.AccountValidation as SA
import Server.Landing.Database as SLD
import Server.Generate as SB
import Server.Token as ST
import Shared.Account (EmailPasswordCaptcha)

registerRegularUser ∷ EmailPasswordCaptcha → ServerEffect String
registerRegularUser epc = do
      email ← SA.validateEmail epc.email
      hash ← SA.validatePassword epc.password
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
            , temporary: false
            }
      ST.createToken id

registerTemporaryUser ∷ ServerEffect String
registerTemporaryUser = do
      name ← SB.generateName
      headline ← SB.generateHeadline
      description ← SB.generateDescription
      id ← SLD.createUser
            { password: Nothing
            , email: Nothing
            , name
            , headline
            , description
            , temporary: true
            }
      ST.createToken id