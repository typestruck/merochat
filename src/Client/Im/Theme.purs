module Client.Im.Theme where

import Prelude

import Client.Im.Flame (NoMessages)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class as EC
import Effect.Uncurried (EffectFn1)
import Effect.Uncurried as EU
import Foreign.Object (Object)
import Foreign.Object as FO
import Shared.Im.Types (Theme(..), ImModel)

foreign import setTheme_ ∷ EffectFn1 String Unit

--this should be with ps, theme switcher likely can be not ffi too (only thing is that it is included in the external pages)
foreign import getTheme_ ∷ Unit → Object String

getTheme ∷ Unit → Array (String /\ String)
getTheme = FO.toUnfoldable <<< getTheme_

setTheme ∷ Theme → ImModel → NoMessages
setTheme theme model = model /\ [ set ]
      where
      set = do
            EC.liftEffect <<< EU.runEffectFn1 setTheme_ $ s theme
            pure Nothing

      s = case _ of
            Dark → "dark"
            Light → "light"
