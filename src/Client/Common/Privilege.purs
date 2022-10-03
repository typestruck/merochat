module Client.Common.Privilege where

import Prelude

import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE

notEnoughKarma :: forall m. m -> Html m
notEnoughKarma action = HE.div (HA.class' "not-enough-karma") [HE.text "You don't have enough karma to unlock this feature yet. Track your progress", HE.a (HA.onClick action) " here"]