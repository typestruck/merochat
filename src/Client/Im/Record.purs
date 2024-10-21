module Client.Im.Record where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn2)
import Effect.Uncurried as EU
import Type.Row.Homogeneous (class Homogeneous)

foreign import data Recorder :: Type

foreign import start_ :: forall r s. EffectFn2 r s Unit

foreign import stop_ :: Effect String

start :: forall r s. Homogeneous r Boolean => Homogeneous s String => Record r -> Record s -> Effect Unit
start constraints options = EU.runEffectFn2 start_ constraints options

stop :: Effect String
stop = stop_