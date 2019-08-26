module Shared.IM.Model where

import Shared.Types

model :: IMUser -> IMModel
model user = IMModel {
        user
}