module Client.IM.Scroll where

import Debug.Trace
import Prelude

import Client.Common.DOM as CCD
import Data.Array as DA
import Data.Foldable as DF
import Data.Traversable as DT
import Effect (Effect)
import Effect.Class.Console as EC
import Shared.Unsafe as SU
import Web.DOM.Element as WDE
import Web.DOM.Internal.Types (Node)
import Web.DOM.MutationObserver (MutationObserver)
import Web.DOM.MutationObserver as WDM
import Web.DOM.MutationRecord (MutationRecordType(..), MutationRecord)
import Web.DOM.MutationRecord as WDM

scrollLastMessage :: Effect Unit
scrollLastMessage = do
      node <- SU.fromJust "scrollTo" <<< WDE.fromNode <<< WDE.toNode <$> CCD.querySelector ".message-history-wrapper"
      CCD.scrollDown node