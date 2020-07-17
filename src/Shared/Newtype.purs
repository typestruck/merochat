module Shared.Newtype where

import Shared.IM.Types
import Shared.Profile.Types

import Data.Newtype as DN
import Shared.Settings.Types (SettingsModel(..))

--REFACTOR: might be more ergonomic to use a lenses package

updateModel model f = DN.over IMModel f model

updateUser user f = DN.over IMUser f user

updateHistoryMessage historyMessage f = DN.over HistoryMessage f historyMessage

updateContact contact f = DN.over Contact f contact

updateProfileModel model f = DN.over ProfileModel f model

updateProfile profileUser f = DN.over ProfileUser f profileUser

updateSettingsModel model f = DN.over SettingsModel f model