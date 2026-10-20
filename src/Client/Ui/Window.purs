module Client.Ui.Window where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1)
import Effect.Uncurried as EU
import Web.HTML as WH
import Web.HTML.Window as WHW

foreign import pushState_ ∷ EffectFn1 String Unit

foreign import screenWidth ∷ Effect Int

foreign import mediaMatches_ ∷ EffectFn1 String Boolean

mediaMatches ∷ String → Effect Boolean
mediaMatches = EU.runEffectFn1 mediaMatches_

confirm ∷ String → Effect Boolean
confirm message = do
      window ← WH.window
      WHW.confirm message window

pushState ∷ String → Effect Unit
pushState = EU.runEffectFn1 pushState_
