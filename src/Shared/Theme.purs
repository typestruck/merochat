module Shared.Theme where

import Prelude

import Client.Lynx.Room as CLR
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as EA
import Effect.Class as EC
import Flame.Html.Event (Event, EventName)
import Flame.Html.Event as FHE
import Flame.Types (NodeData)

foreign import onThemeRequested ∷ ((String → Effect Unit) → Effect Unit) → Effect Unit

foreign import onThemeChanged ∷ (String → Effect Unit) → Effect Unit

foreign import systemTheme ∷ Effect String

foreign import handleEvent ∷ String → Event → Effect Unit

runThemeEvent ∷ ∀ message. String → EventName → NodeData message
runThemeEvent root name = FHE.createRawEvent name $ \value → handleEvent root value $> Nothing

onLayoutChange ∷ ∀ message. String → NodeData message
onLayoutChange root = runThemeEvent root "bindlayoutchange"

initialize ∷ Effect Unit
initialize = do
      onThemeRequested $ \apply → EA.launchAff_ do
            storedTheme ← CLR.select ∷ Aff (Maybe { theme ∷ String })
            EC.liftEffect do
                  theme ← case storedTheme of
                        Just { theme } | theme == "light" || theme == "dark" → pure theme
                        _ → systemTheme
                  apply theme
      onThemeChanged $ \theme → EA.launchAff_ $ CLR.insert { theme }
