module Client.Im.Record where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1)
import Effect.Uncurried as EU
import Type.Row.Homogeneous (class Homogeneous)

foreign import data Recorder :: Type

foreign import start_ :: forall r. EffectFn1 r Unit

foreign import stop_ :: Effect String

start :: forall r. Homogeneous r Boolean => Record r -> Effect Unit
start options = EU.runEffectFn1 start_ options

stop :: Effect String
stop = stop_