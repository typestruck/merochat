module Shared.IM.Contact where

import Shared.IM.Types (Contact(..), IMUser(..))
import Shared.Types (PrimaryKey(..))

defaultContact :: PrimaryKey -> IMUser -> Contact
defaultContact id chatted = Contact { user: chatted, chatStarter: id, history: [], chatAge: 0.0 }