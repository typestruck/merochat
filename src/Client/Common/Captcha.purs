module Client.Common.Captcha where

import Prelude

import Data.Nullable (Nullable)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn3)
import Effect.Uncurried as EU

type Parameters =
      { sitekey ∷ String
      , size ∷ String
      , callback ∷ String → Effect Unit
      }

foreign import execute_ ∷ EffectFn1 (Nullable String) Unit
foreign import reset_ ∷ EffectFn1 (Nullable String) Unit
foreign import render_ ∷ EffectFn3 String Parameters Boolean Unit

defaultParameters ∷ (String → Effect Unit) → Parameters
defaultParameters callback = { sitekey: "6LeDyE4UAAAAABhlkiT86xpghyJqiHfXdGZGJkB0", callback, size: "invisible" }

execute ∷ Nullable String → Effect Unit
execute = EU.runEffectFn1 execute_

reset ∷ Nullable String → Effect Unit
reset = EU.runEffectFn1 reset_

render ∷ String → Parameters → Boolean → Effect Unit
render = EU.runEffectFn3 render_
