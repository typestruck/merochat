module Shared.External.Types where

import Prelude

data Page = Landing | Login | Help | Terms | Privacy | Backer | Recover | Banned

derive instance Eq Page
