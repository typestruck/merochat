module Server.Profile.Types where

import Prelude

import Shared.Profile.Types (ProfileUser)

type Payload = { user ∷ ProfileUser, countries ∷ Array { id ∷ Int, name ∷ String }, languages ∷ Array { id ∷ Int, name ∷ String } }