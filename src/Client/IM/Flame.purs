module Client.IM.Flame where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Flame ((:>))
import Flame as F
import Flame.Application.Effectful as FAE
import Flame.Application.Effectful as FAE
import Shared.IM.Types (IMMessage, IMModel(..))

-- | This action does has no further effects
type NoMessages = Tuple IMModel (Array (Aff (Maybe IMMessage)))

-- | This action does has further effects
type MoreMessages = Tuple IMModel (Array (Aff (Maybe IMMessage)))

-- | This action does has a single further effect
type NextMessage = Tuple IMModel (Array (Aff (Maybe IMMessage)))

-- | Helper to clean up updating a model when no new messages will be raised
diff fields = F.noMessages <<< FAE.diff' fields

-- | Helper to clean up updating a model when new messages will be raised
diffNext fields affs model = FAE.diff' fields model :> affs

-- | Same as pure <<< Just
next = pure <<< Just

-- | Helper to raise a new message from an effect
nextMessage message effect model = model :> [ (Just <<< message) <$> effect ]

-- | Helper to raise a single message without performing effects
justNext model message = model :> [pure $ Just message]

-- Helper to perform a single effect that does not raise a new message
nothingNext model aff = model :> [
        do aff
        pure Nothing
]
