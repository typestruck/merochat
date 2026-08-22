module Client.EventTypes where

import Web.Event.Event (EventType(..))

-- | Event for displaying / hiding a modal
-- |
-- | This event is always triggered from the im page and not the modals themselves
modalVisible ∷ EventType
modalVisible = EventType "modal-visible"

toQuote ∷ EventType
toQuote = EventType "ToQuote"