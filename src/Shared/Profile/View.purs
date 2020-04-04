module Shared.Profile.View where

import Prelude
import Shared.IM.Types
import Shared.Profile.Types
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String.Common as DSC
import Shared.Unsafe((!@))
import Shared.Unsafe as SU
import Flame (Html)
import Data.Tuple(Tuple(..))
import Flame.HTML.Attribute as HA
import Data.Int53 as DI
import Data.Newtype as DN
import Debug.Trace (spy)
import Data.Enum as DE
import Data.Foldable as DF
import Debug.Trace(spy)
import Flame.HTML.Element as HE
import Data.Array as DA

view :: ProfileUser -> Html ProfileMessage
view profileUser = HE.div (HA.class' "im") [
        HE.div (HA.class' "chat-box") [
                profile profileUser
        ]
]

profile :: ProfileUser -> Html ProfileMessage
profile model = HE.text "OI"
