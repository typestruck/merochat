module Server.Profile.Types where

import Prelude

import Shared.Post (Post)
import Shared.Profile.Types (ProfileUser)

type Payload =
      { user ∷ ProfileUser
      , posts ∷ Array Post
      , countries ∷ Array { id ∷ Int, name ∷ String }
      , languages ∷ Array { id ∷ Int, name ∷ String }
      }