module Client.Ui.Element where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2)
import Effect.Uncurried as EU
import Web.DOM.Element (Element)

foreign import innerHTML_ ∷ EffectFn2 Element String Unit

foreign import removeFromClassList_ ∷ EffectFn2 Element String Unit

foreign import addToClassList_ ∷ EffectFn2 Element String Unit

foreign import value_ ∷ EffectFn1 Element String

foreign import setValue_ ∷ EffectFn2 Element String Unit

foreign import toggleDisabled_ ∷ EffectFn1 Element Unit

-- | Field value property or button inner text
value ∷ Element → Effect String
value element = EU.runEffectFn1 value_ element

setValue ∷ Element → String → Effect Unit
setValue element value = EU.runEffectFn2 setValue_ element value

toggleDisabled ∷ Element → Effect Unit
toggleDisabled = EU.runEffectFn1 toggleDisabled_

setInnerHTML ∷ Element → String → Effect Unit
setInnerHTML element = EU.runEffectFn2 innerHTML_ element

removeFromClassList ∷ Element → String → Effect Unit
removeFromClassList = EU.runEffectFn2 removeFromClassList_

addToClassList ∷ Element → String → Effect Unit
addToClassList = EU.runEffectFn2 addToClassList_
