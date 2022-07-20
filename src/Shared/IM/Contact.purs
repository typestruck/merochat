module Shared.IM.Contact where

import Prelude
import Shared.ContentType
import Shared.IM.Types

import Data.Array ((!!))
import Data.Maybe (Maybe(..))
import Effect.Now as EN
import Effect.Unsafe as EU
import Shared.DateTime (DateTimeWrapper(..))
import Shared.Unsafe as SU

defaultContact ∷ Int → ImUser → Contact
defaultContact id chatted =
      { shouldFetchChatHistory: false
      , user: chatted
      , lastMessageDate: DateTimeWrapper $ EU.unsafePerformEffect EN.nowDateTime
      , impersonating: Nothing
      , chatStarter: id
      , history: []
      , chatAge: 0.0
      , typing: false
      }

chattingContact ∷ Array Contact → Maybe Int → Contact
chattingContact contacts chatting = SU.fromJust do
      index ← chatting
      contacts !! index