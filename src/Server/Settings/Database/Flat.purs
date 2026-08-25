module Server.Settings.Database.Flat where

import Data.Maybe (Maybe)
import Safe.Coerce as SC
import Server.Database.Types (Checked(..))
import Shared.Settings.Types (UserSettings)
import Shared.User (ProfileVisibility)
import Shared.SuggestionsFrom (SuggestionsFrom)

toUserSettings ∷
      { readReceipts ∷ Checked
      , chatBackground ∷ Maybe String
      , ownBackground ∷ Checked
      , typingStatus ∷ Checked
      , profileVisibility ∷ ProfileVisibility
      , asksVisibility ∷ ProfileVisibility
      , onlineStatus ∷ Checked
      , messageTimestamps ∷ Checked
      , lastMessageOnContactList ∷ Checked
      , postsVisibility ∷ ProfileVisibility
      , suggestionsFrom ∷ SuggestionsFrom
      } →
      UserSettings
toUserSettings p = SC.coerce p