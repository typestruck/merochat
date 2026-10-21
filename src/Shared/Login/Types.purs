module Shared.Login.Types where

import Data.Maybe (Maybe)
import Shared.Im.Types (Contact)
import Shared.User (User)

type LoginResponse = { user ∷ Maybe User, contacts ∷ Array Contact }

type LoginModel =
      { email ∷ Maybe String
      , password ∷ Maybe String
      , loading ∷ Boolean
      , error ∷ Maybe String
      }

data LoginMessage
      = SetEmail (Maybe String)
      | SetPassword (Maybe String)
      | Submit
      | KeyPressed String
      | Failed String
      | Reset
