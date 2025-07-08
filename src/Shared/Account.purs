module Shared.Account where

type EmailCaptcha =
      { email ∷ String
      , captchaResponse ∷ String
      }

type EmailPassword =
      { email ∷ String
      , password ∷ String
      }

type EmailPasswordCaptcha =
      { email ∷ String
      , password ∷ String
      , captchaResponse ∷ String
      }

type RegisterTemporary = { captchaResponse ∷ String }

type ResetPassword =
      { token ∷ String
      , password ∷ String
      }
