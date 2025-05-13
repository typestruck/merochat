module Shared.Im.Contact where

import Prelude

import Data.Array as DA
import Data.Maybe (Maybe(..))
import Effect.Now as EN
import Effect.Unsafe as EU
import Shared.DateTime (DateTimeWrapper(..))
import Shared.Im.Types (Contact, User)

defaultContact ∷ Int → User → Contact
defaultContact id chatted =
      { shouldFetchChatHistory: false
      , user: chatted
      , lastMessageDate: DateTimeWrapper $ EU.unsafePerformEffect EN.nowDateTime
      , chatStarter: id
      , history: []
      , scrollChatDown: true
      , chatAge: 0.0
      , typing: false
      }

findContact ∷ Int → Array Contact → Maybe Contact
findContact userId contacts = DA.find f contacts
      where
      f contact = contact.user.id == userId

maybeFindContact ∷ Maybe Int → Array Contact → Maybe Contact
maybeFindContact ui contacts = case ui of
      Nothing → Nothing
      Just userId → findContact userId contacts