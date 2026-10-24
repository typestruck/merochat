module Shared.Landing.Types where

import Data.Maybe (Maybe)

type LandingModel =
      { email ∷ Maybe String
      , password ∷ Maybe String
      , loading ∷ Boolean
      , error ∷ Maybe String
      }

data LandingMessage
      = SetEmail (Maybe String)
      | SetPassword (Maybe String)
      | Submit
      | Guest
      | Failed String
      | Reset
