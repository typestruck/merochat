module Server.Settings.Database.Flat where

import Data.Maybe (Maybe)
import Safe.Coerce as SC
import Server.Database.Types (Checked(..))
import Shared.Settings.Types (UserSettings)
import Shared.User (ProfileVisibility)

toUserSettings ∷
      { readReceipts ∷ Checked
      , chatBackground :: Maybe String
      , ownBackground :: Checked
      , typingStatus ∷ Checked
      , profileVisibility ∷ ProfileVisibility
      , onlineStatus ∷ Checked
      , messageTimestamps ∷ Checked
      , postsVisibility ∷ ProfileVisibility
      } →
      UserSettings
toUserSettings p = SC.coerce p