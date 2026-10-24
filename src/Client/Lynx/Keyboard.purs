module Client.Lynx.Keyboard where

import Prelude

import Control.Monad.Except as CME
import Data.Either as DE
import Data.Maybe (Maybe)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Flame.Types (Subscription)
import Foreign (Foreign)
import Foreign as F
import Foreign.Index as FI

foreign import initialize_ ∷ String → Effect Unit

eventName ∷ String
eventName = "merochatkeyboardchanged"

initialize ∷ Effect Unit
initialize = initialize_ eventName

onChange ∷ ∀ message. (Number → message) → Subscription message
onChange message = eventName /\ (pure <<< map message <<< keyboardHeight)

keyboardHeight ∷ Foreign → Maybe Number
keyboardHeight event = DE.hush <<< CME.runExcept $ do
      status ← FI.readProp "status" event >>= F.readString
      if status == "on" then
            FI.readProp "height" event >>= F.readNumber
      else
            pure 0.0
