module Client.Lynx.ChoicePicker (pick) where

import Prelude

import Client.Lynx.Room as CLR
import Data.Maybe (Maybe)
import Data.Maybe as DM
import Data.Nullable (Nullable)
import Data.Nullable as DN
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff)
import Effect.Aff.Compat as EAC

foreign import pick_ ∷ String → Nullable String → Array { value ∷ String, name ∷ String } → String → EffectFnAff (Nullable String)

pick ∷ String → Maybe String → Array { value ∷ String, name ∷ String } → Aff (Maybe String)
pick title initial options = do
      stored ← CLR.select ∷ Aff (Maybe { theme ∷ String })
      result ← EAC.fromEffectFnAff $ pick_ title (DN.toNullable initial) options (DM.maybe "" _.theme stored)
      pure $ DN.toMaybe result
