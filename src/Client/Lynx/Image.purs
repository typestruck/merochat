module Client.Lynx.Image (pick) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Nullable as DN
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff)
import Effect.Aff.Compat as EAC

foreign import pick_ ∷ EffectFnAff (Nullable String)

pick ∷ Aff (Maybe String)
pick = DN.toMaybe <$> EAC.fromEffectFnAff pick_
