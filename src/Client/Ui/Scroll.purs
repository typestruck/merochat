module Client.Ui.Scroll where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1)
import Effect.Uncurried as EU
import Shared.Element (ElementId)
import Web.DOM.Element (Element)
import Web.DOM.Element as WDE

foreign import scrollIntoView_ ∷ EffectFn1 Element Unit

foreign import scrollToTop_ ∷ EffectFn1 String Unit

scrollToTop ∷ ElementId → Effect Unit
scrollToTop = show >>> EU.runEffectFn1 scrollToTop_

scrollDown ∷ Element → Effect Unit
scrollDown element = do
      height ← WDE.scrollHeight element
      WDE.setScrollTop height element

scrollIntoView ∷ Element → Effect Unit
scrollIntoView element = EU.runEffectFn1 scrollIntoView_ element
