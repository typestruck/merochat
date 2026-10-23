module Client.Lynx.DatePicker (pick) where

import Prelude

import Data.Maybe (Maybe)
import Data.Maybe as DM
import Data.Nullable (Nullable)
import Data.Nullable as DN
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff)
import Effect.Aff.Compat as EAC
import Shared.DateTime (DateWrapper(..))
import Shared.DateTime as SD

foreign import pick_ ∷ String → EffectFnAff (Nullable String)

pick ∷ Maybe DateWrapper → Aff (Maybe DateWrapper)
pick initial = do
      result ← EAC.fromEffectFnAff <<< pick_ $ DM.maybe "" SD.formatIsoDate initial
      pure $ DN.toMaybe result >>= map DateWrapper <<< SD.unformatIsoDate
