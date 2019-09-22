module Shared.IM.Model where

import Shared.Types

import Data.Maybe (Maybe(..))

model :: Array IMUser -> IMUser -> IMModel
model suggestions user = IMModel {
        user,
        suggestions,
        chatting: Nothing
}