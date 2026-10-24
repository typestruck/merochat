module Shared.Recover.Types where

import Data.Maybe (Maybe)

type RecoverModel = { email ∷ Maybe String, password ∷ Maybe String, confirmation ∷ Maybe String, token ∷ Maybe String, loading ∷ Boolean, error ∷ Maybe String, success ∷ Maybe String }

data RecoverMessage = SetEmail (Maybe String) | SetPassword (Maybe String) | SetConfirmation (Maybe String) | SetToken (Maybe String) | Submit | Finished String | Failed String

