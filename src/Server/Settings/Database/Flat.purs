module Server.Settings.Database.Flat where

import Safe.Coerce as SC
import Server.Database.Types (Checked(..))
import Shared.Settings.Types (PrivacySettings)
import Shared.User (ProfileVisibility)

toPrivacySettings ∷
      { readReceipts ∷ Checked
      , typingStatus ∷ Checked
      , profileVisibility ∷ ProfileVisibility
      , onlineStatus ∷ Checked
      , messageTimestamps ∷ Checked
      } →
      PrivacySettings
toPrivacySettings p = SC.coerce p