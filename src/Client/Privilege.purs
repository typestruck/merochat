module Client.Privilege where

import Prelude

import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Extra as SE

notEnoughKarma ∷ ∀ m. String → m → Html m
notEnoughKarma for action = HE.div [ HA.class' "not-enough-karma" ]
      [ HE.text $ "You don't have enough karma to " <> for <> " yet. Track your progress"
      , SE.a [ HA.onClick action ] [ HE.text " here" ]
      ]