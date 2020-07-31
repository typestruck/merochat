module Shared.IM.Contact where

import Data.Array ((!!))
import Data.Maybe (Maybe)
import Shared.IM.Types (Contact(..), IMUser(..))
import Shared.Types (PrimaryKey(..))
import Shared.Unsafe as SU
import Prelude

defaultContact :: PrimaryKey -> IMUser -> Contact
defaultContact id chatted = Contact {
        shouldFetchChatHistory: false,
        user: chatted,
        chatStarter: id,
        history: [],
        chatAge: 0.0
}

chattingContact :: Array Contact -> Maybe Int -> Contact
chattingContact contacts chatting = SU.fromJust do
        index <- chatting
        contacts !! index