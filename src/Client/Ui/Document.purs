module Client.Ui.Document where

import Prelude

import Data.Maybe (Maybe)
import Data.Maybe as DM
import Effect (Effect)
import Effect.Exception as EE
import Shared.Element (ElementId)
import Shared.Resource (Bundle, ResourceType(..))
import Shared.Resource as SR
import Shared.Unsafe as SU
import Web.DOM.Document as WDD
import Web.DOM.Element (Element)
import Web.DOM.Element as WDE
import Web.DOM.Node as WDN
import Web.DOM.NonElementParentNode as WDNEPN
import Web.DOM.ParentNode (QuerySelector(..))
import Web.DOM.ParentNode as WDP
import Web.HTML as WH
import Web.HTML.HTMLDocument as WHHD
import Web.HTML.HTMLElement as WHHE
import Web.HTML.HTMLScriptElement as WHHS
import Web.HTML.Window as WHW

foreign import documentHasFocus ∷ Effect Boolean

foreign import documentIsNotHidden ∷ Effect Boolean

-- | Selects a single element
unsafeGetElementById ∷ ElementId → Effect Element
unsafeGetElementById elementId = do
      maybeElement ← getElementById elementId
      DM.maybe (EE.throwException $ EE.error $ "No element with id:" <> show elementId) pure maybeElement

getElementById ∷ ElementId → Effect (Maybe Element)
getElementById elementId = do
      window ← WH.window
      document ← WHHD.toDocument <$> WHW.document window
      WDNEPN.getElementById (show elementId) $ WDD.toNonElementParentNode document

loadScript ∷ Bundle → Effect Unit
loadScript resource = do
      window ← WH.window
      document ← WHW.document window
      script ← WDD.createElement "script" $ WHHD.toDocument document
      WHHS.setSrc (SR.bundlePath resource Js) <<< SU.fromJust $ WHHS.fromElement script
      body ← SU.fromJust <$> WHHD.body document
      void <<< WDN.appendChild (WDE.toNode script) $ WHHE.toNode body

-- | Selects a single element.
unsafeQuerySelector ∷ String → Effect Element
unsafeQuerySelector selector = do
      maybeElement ← querySelector selector
      DM.maybe (EE.throwException $ EE.error $ "Selector returned no nodes:" <> selector) pure maybeElement

querySelector ∷ String → Effect (Maybe Element)
querySelector selector = do
      window ← WH.window
      document ← WHHD.toDocument <$> WHW.document window
      WDP.querySelector (QuerySelector selector) $ WDD.toParentNode document

createElement ∷ String → Effect Element
createElement tag = do
      window ← WH.window
      document ← WHW.document window
      WDD.createElement tag $ WHHD.toDocument document

setTitle ∷ String → Effect Unit
setTitle title = do
      window ← WH.window
      document ← WHW.document window
      WHHD.setTitle title document
