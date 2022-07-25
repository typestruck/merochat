module Server.IM.Types where

import Prelude

import Shared.IM.Types (Contact, Suggestion, ImUser)

type Payload = { contacts ∷ Array Contact, suggestions ∷ Array Suggestion, user :: ImUser }