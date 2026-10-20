module Shared.Login.Types where

type LoginModel =
      { email ∷ String
      , password ∷ String
      , loading ∷ Boolean
      , error ∷ String
      }

data LoginMessage
      = SetEmail String
      | SetPassword String
      | Submit
      | KeyPressed String
      | Failed String
      | LoggedIn
