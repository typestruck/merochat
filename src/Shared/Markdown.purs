module Shared.Markdown(toHTML, toRestrictedHTML) where

import Prelude

import Effect.Uncurried (EffectFn2)
import Flame.Types (VNode)

foreign import parseRestricted :: String -> EffectFn2 VNode VNode Unit
foreign import parse :: String -> EffectFn2 VNode VNode Unit

toHTML :: String -> EffectFn2 VNode VNode Unit
toHTML = parse

toRestrictedHTML :: String -> EffectFn2 VNode VNode Unit
toRestrictedHTML = parseRestricted