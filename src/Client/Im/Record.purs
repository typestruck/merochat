module Client.Im.Record where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn3)
import Effect.Uncurried as EU
import Flame.Subscription as FS
import Shared.Im.Types (ImMessage)
import Shared.Options.MountPoint (imId)
import Type.Row.Homogeneous (class Homogeneous)

foreign import data Recorder ∷ Type

foreign import start_ ∷ ∀ r s. EffectFn3 r s (EffectFn1 String Unit) Unit

foreign import stop_ ∷ Effect Unit

start ∷ ∀ r s. Homogeneous r Boolean ⇒ Homogeneous s String ⇒ Record r → Record s → (String → ImMessage) → Effect Unit
start constraints options message = EU.runEffectFn3 start_ constraints options (EU.mkEffectFn1 handler)
      where
      handler s = FS.send imId (message s)

stop ∷ Effect Unit
stop = stop_