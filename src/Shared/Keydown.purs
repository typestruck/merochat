module Shared.Keydown where

import Prelude

import Data.Maybe (Maybe(..))
import Flame (Key)
import Flame.Html.Event as HA
import Flame.Types (NodeData)
import Shared.Types (IMMessage)
import Shared.Unsafe as SU
import Web.UIEvent.KeyboardEvent as WUK

keyDownOn :: Key -> IMMessage -> NodeData IMMessage
keyDownOn keyName message = HA.createRawEvent "keydown" handler
      where handler event = do
                  let   keyboardEvent = SU.fromJust $ WUK.fromEvent event
                        key = WUK.key keyboardEvent
                  pure $ if key == keyName && not WUK.shiftKey keyboardEvent then Just message else Nothing
