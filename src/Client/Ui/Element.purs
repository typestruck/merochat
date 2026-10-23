module Client.Ui.Element where

import Prelude

import Control.Promise (Promise)
import Control.Promise as CP
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Uncurried (EffectFn1, EffectFn2)
import Effect.Uncurried as EU
import Web.DOM.Element (Element)

foreign import innerHTML_ ∷ EffectFn2 Element String Unit

foreign import removeFromClassList_ ∷ EffectFn2 Element String Unit

foreign import addToClassList_ ∷ EffectFn2 Element String Unit

foreign import value_ ∷ EffectFn1 Element (Promise String)

foreign import setValue_ ∷ EffectFn2 Element String (Promise Unit)

foreign import toggleDisabled_ ∷ EffectFn1 Element Unit

-- | Field value property or button inner text
value ∷ Element → Aff String
value = EU.runEffectFn1 value_ >>> CP.toAffE

setValue ∷ Element → String → Aff Unit
setValue element = EU.runEffectFn2 setValue_ element >>> CP.toAffE

toggleDisabled ∷ Element → Effect Unit
toggleDisabled = EU.runEffectFn1 toggleDisabled_

setInnerHTML ∷ Element → String → Effect Unit
setInnerHTML element = EU.runEffectFn2 innerHTML_ element

removeFromClassList ∷ Element → String → Effect Unit
removeFromClassList = EU.runEffectFn2 removeFromClassList_

addToClassList ∷ Element → String → Effect Unit
addToClassList = EU.runEffectFn2 addToClassList_

foreign import focus_ ∷ EffectFn1 Element (Promise Unit)

foreign import selectionEnd_ ∷ EffectFn1 Element (Promise Int)

foreign import setSelection_ ∷ EffectFn2 Element Int (Promise Unit)

focus ∷ Element → Aff Unit
focus = EU.runEffectFn1 focus_ >>> CP.toAffE

selectionEnd ∷ Element → Aff Int
selectionEnd = EU.runEffectFn1 selectionEnd_ >>> CP.toAffE

setSelection ∷ Element → Int → Aff Unit
setSelection element = EU.runEffectFn2 setSelection_ element >>> CP.toAffE
