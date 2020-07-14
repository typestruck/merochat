module Client.IM.Scroll where

import Web.DOM.MutationObserver as WDM
import Web.DOM.MutationObserver (MutationObserver)
import Web.DOM.MutationRecord (MutationRecordType(..), MutationRecord)
import Web.DOM.MutationRecord as WDM
import Web.DOM.Internal.Types (Node)
import Prelude
import Shared.Unsafe as SU
import Data.Traversable as DT
import Web.DOM.Element as WDE
import Client.Common.DOM as CCD
import Data.Array as DA
import Data.Foldable as DF
import Effect(Effect)
import Debug.Trace

scrollLastMessage :: Effect Unit
scrollLastMessage = do
      node <- map WDE.toNode $ CCD.querySelector ".message-history-wrapper"
      observer <- WDM.mutationObserver (scrollTo node)
      WDM.observe node {childList: true} observer

      where scrollTo node mutations _ = do
                  mutationTypes <- DT.traverse (map (_ == "childList") <<< WDM.typeString) mutations
                  when (DF.or mutationTypes) $ CCD.scrollDown (SU.unsafeFromJust "scrollTo" $ WDE.fromNode node)