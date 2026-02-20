module Client.Im.Praise where

import Prelude

import Client.Im.Flame (NoMessages)
import Client.Network (routes)
import Client.Network as CCN
import Data.Array ((:))
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Shared.Im.Types (ImMessage(..), ImModel)
import Shared.Praise (PraisedFor(..))
import Shared.Unsafe as SU

togglePraise ∷ Int → PraisedFor → ImModel → NoMessages
togglePraise userId for model = model { praise = model.praise { selected = Just (userId /\ toggle model.praise.selected) } } /\ []
      where
      toggle = case _ of
            Nothing → [ for ]
            Just (_ /\ selected) → if DA.elem for selected then DA.filter (_ /= for) selected else for : selected

setOtherPraise ∷ Maybe String → ImModel → NoMessages
setOtherPraise praise model = model { praise = model.praise { other = praise } } /\ []

savePraise ∷ ImModel → NoMessages
savePraise model = model { praise = model.praise { freeToSave = false } } /\ [ save ]
      where
      userId /\ for = SU.fromJust model.praise.selected
      save = do
            response ← CCN.silentRequest $ routes.praise.save { body: { for: DA.filter (_ /= Other "") for, userId } }
            pure <<< Just $ AfterSavePraise userId response.allowed

afterSavePraise ∷ Int → Boolean → ImModel → NoMessages
afterSavePraise userId allowed model = model { praise = model.praise { freeToSave = true, selected = Nothing } } /\ []
