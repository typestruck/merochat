module Client.IM.Flame where

import Prelude
import Shared.Im.Types
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Effect.Aff (Aff)
import Flame ((:>))
import Flame as F
import Flame.Application.Effectful as FAE

-- | This action has no further effects
type NoMessages = Tuple ImModel (Array (Aff (Maybe ImMessage)))

-- | This action has further effects
type MoreMessages = Tuple ImModel (Array (Aff (Maybe ImMessage)))

-- | This action has a single further effect
type NextMessage = Tuple ImModel (Array (Aff (Maybe ImMessage)))

-- | Same as pure <<< Just
next = pure <<< Just

-- | Helper to raise a new message without performing an effect
justNext ∷ ImModel → ImMessage → NextMessage
justNext model message = model :> [ pure <<< Just $ message ]

-- Helper to perform a single effect that does not raise a new message
nothingNext ∷ ImModel → Aff Unit → Tuple ImModel (Array (Aff (Maybe ImMessage)))
nothingNext model aff = model :>
      [ do
              aff
              pure Nothing
      ]
