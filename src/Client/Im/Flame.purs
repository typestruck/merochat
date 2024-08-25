module Client.Im.Flame where

import Prelude
import Shared.Im.Types
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Effect.Aff (Aff)

-- | This action has no further effects
type NoMessages = Tuple ImModel (Array (Aff (Maybe ImMessage)))

-- | This action has further effects
type MoreMessages = Tuple ImModel (Array (Aff (Maybe ImMessage)))

-- | This action has a single further effect
type NextMessage = Tuple ImModel (Array (Aff (Maybe ImMessage)))

