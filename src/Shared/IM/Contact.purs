module Shared.IM.Contact where

import Data.Array ((!!))
import Data.Maybe (Maybe(..))
import Shared.ContentType
import Shared.IM.Types
import Shared.Unsafe as SU
import Prelude

defaultContact ∷ Int → ImUser → Contact
defaultContact id chatted =
      {  shouldFetchChatHistory: false
      , user: chatted
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