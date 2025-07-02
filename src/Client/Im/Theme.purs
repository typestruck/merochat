module Client.Im.Theme where

import Prelude

import Client.Im.Flame (NoMessages)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Class as EC
import Effect.Uncurried (EffectFn1)
import Effect.Uncurried as EU
import Shared.Im.Types (Theme(..), ImModel)


foreign import setTheme_ :: EffectFn1 String Unit

setTheme :: Theme -> ImModel -> NoMessages
setTheme theme model = model /\ [  set  ]
    where
    set = do
            EC.liftEffect <<< EU.runEffectFn1 setTheme_ $ s theme
            pure Nothing

    s = case _ of
        Dark -> "dark"
        Light -> "light"