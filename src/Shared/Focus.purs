module Shared.Focus where

import Prelude

import Effect.Uncurried (EffectFn2)
import Flame.Renderer.Hook as FRH
import Flame.Types (NodeData)

--foreign import focus_ :: EffectFn2 VNode VNode Unit

-- focus :: forall m. NodeData m
-- focus = FRH.atPostpatch focus_