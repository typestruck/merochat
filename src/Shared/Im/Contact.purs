module Shared.Im.Contact where

import Prelude
import Shared.Im.Types (Contact, ImUser)

import Data.Array ((!!))
import Data.Maybe (Maybe)
import Effect.Now as EN
import Effect.Unsafe as EU
import Shared.DateTime (DateTimeWrapper(..))
import Shared.Unsafe as SU

defaultContact ∷ Int → ImUser → Contact
defaultContact id chatted =
      { shouldFetchChatHistory: false
      , user: chatted
      , lastMessageDate: DateTimeWrapper $ EU.unsafePerformEffect EN.nowDateTime
      , chatStarter: id
      , history: []
      , chatAge: 0.0
      , typing: false
      }