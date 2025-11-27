module Shared.Post where

import Data.Maybe (Maybe)
import Shared.DateTime (DateTimeWrapper)
import Shared.Content (Content)

type Post =
      { id ∷ Int
      , date ∷ DateTimeWrapper
      , content ∷ String
      , expires ∷ Maybe DateTimeWrapper
      }

type PostPayload =
      { content ∷ Content
      }