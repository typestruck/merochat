module Shared.Account where

import Data.Maybe (Maybe)

type EmailCaptcha r =
      { email ∷ String
      , captchaResponse ∷ Maybe String
      | r
      }

-- | Fields for registration or login
type RegisterLogin = (EmailCaptcha (password ∷ String))

type RegisterTemporary = { captchaResponse ∷ Maybe String }

type RegisterLoginUser =
      { id ∷ Int
      , email ∷ Maybe String
      , password ∷ Maybe String
      }

type RecoverAccount = EmailCaptcha ()

type ResetPassword =
      { token ∷ String
      , password ∷ String
      }
