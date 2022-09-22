module Shared.Setter where

import Prelude
import Shared.Im.Types

import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Prim.Row (class Cons, class Lacks, class Nub, class Union)
import Record as R
import Type.Proxy (Proxy(..))

setUserField field value model = model
      { user = R.set field value model.user
      }

setIMField ∷ ∀ field r v. IsSymbol field ⇒ Cons field v r Im ⇒ Proxy field → v → ImMessage
setIMField field = SetField <<< R.set field


setJust ∷ ∀ t7 t8 t9. IsSymbol t8 ⇒ Cons t8 (Maybe t9) t7 Im ⇒ Proxy t8 → t9 → ImMessage
setJust field = setIMField field <<< Just