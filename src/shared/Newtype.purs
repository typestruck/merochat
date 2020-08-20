module Shared.Newtype where

import Shared.Types
import Prelude

import Data.Newtype as DN

updateModel model f = DN.over IMModel f model

updateUserWrapper user f = DN.over IMUserWrapper f user

updateHistoryMessageWrapper historyMessage f = DN.over HistoryMessageWrapper f historyMessage

updateContactWrapper contact f = DN.over ContactWrapper f contact

updateProfileModel model f = DN.over ProfileModel f model

updateProfile profileUser f = DN.over ProfileUser f profileUser

updateSettingsModel model f = DN.over SettingsModel f model

unwrapAll = map (map DN.unwrap)