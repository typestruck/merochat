module Server.Im.Types where

import Prelude

import Shared.Im.Types (Contact, Suggestion, User)

type Payload = { contacts ∷ Array Contact, suggestions ∷ Array Suggestion, user ∷ User }