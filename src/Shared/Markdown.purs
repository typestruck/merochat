module Shared.Markdown(displayMarkdown, displayRestrictedMarkdown) where

import Prelude

import Debug.Trace (spy)
import Effect.Uncurried (EffectFn2)
import Flame.HTML.Attribute as HA
import Flame.HTML.Element as HE
import Flame.Renderer.Hook as FRH
import Flame.Types (Html, VNode)

--snabbdom hooks dont work server side so we need innerHTML server side
foreign import parseRestricted :: String -> String
foreign import parse :: String -> String

foreign import parseHook :: String -> EffectFn2 VNode VNode Unit
foreign import parseRestrictedHook :: String -> EffectFn2 VNode VNode Unit

displayMarkdown :: forall message. { markdown :: String, extraClasses :: String, useHooks :: Boolean } -> Html message
displayMarkdown { markdown, extraClasses, useHooks } = HE.div' [HA.class' extraClasses, renderer]
      where renderer
                  | useHooks = FRH.atPostpatch $ parseHook markdown
                  | otherwise = HA.innerHTML $ parse  markdown

displayRestrictedMarkdown :: forall message. { markdown :: String, extraClasses :: String, useHooks :: Boolean } -> Html message
displayRestrictedMarkdown { markdown, extraClasses, useHooks } = HE.div' [HA.class' extraClasses, renderer]
      where renderer
                  | useHooks = FRH.atPostpatch $ parseRestrictedHook markdown
                  | otherwise = HA.innerHTML $ parseRestricted markdown