module Shared.Dropdown (choices, dropdown, pick) where

import Prelude

import Client.Lynx.Room as CLR
import Data.Array as DA
import Data.Int as DI
import Data.Maybe (Maybe)
import Data.Maybe as DM
import Data.Nullable (Nullable)
import Data.Nullable as DN
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff)
import Effect.Aff.Compat as EAC
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE

foreign import pick_ ∷ String → Nullable String → Array { value ∷ String, name ∷ String } → String → EffectFnAff (Nullable String)

--the native component to choose something from a list of options
pick ∷ String → Maybe String → Array { value ∷ String, name ∷ String } → Aff (Maybe String)
pick title initial options = do
      stored ← CLR.select ∷ Aff (Maybe { theme ∷ String })
      result ← EAC.fromEffectFnAff $ pick_ title (DN.toNullable initial) options (DM.maybe "" _.theme stored)
      pure $ DN.toMaybe result

type Select message = String → Maybe String → Array { value ∷ String, name ∷ String } → (String → message) → message

dropdown ∷ ∀ message. Select message → String → Maybe String → String → (String → message) → Array { value ∷ String, name ∷ String } → Html message
dropdown select title selected label update options = HE.div [ HA.class' "profile-choice" ]
      [ HE.button [ HA.class' "modal-select", HA.onClick $ select title selected options update ] [ HE.text label ] ]

choices ∷ ∀ value message. Eq value ⇒ Select message → String → value → (value → message) → Array { value ∷ value, name ∷ String } → Html message
choices select title selected update options = dropdown select title (show <$> index) label choose indexed
      where
      index = DA.findIndex ((_ == selected) <<< _.value) options
      label = DM.maybe title _.name $ index >>= DA.index options
      indexed = DA.mapWithIndex (\i option → { value: show i, name: option.name }) options
      choose value = update <<< DM.fromMaybe selected $ _.value <$> (DI.fromString value >>= DA.index options)
