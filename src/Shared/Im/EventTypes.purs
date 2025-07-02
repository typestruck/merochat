module Shared.Im.EventTypes where

import Prelude

import Web.Event.Event (EventType(..))

modalVisible ∷ EventType
modalVisible = EventType "modal-visible"