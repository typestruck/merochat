module Client.IM.Main where

import Prelude
import Shared.Types

import Client.IM.Suggestion as CIS
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Flame (QuerySelector(..), World)
import Flame as F
import Shared.IM.View as SIV

main :: Effect Unit
main =
        F.resumeMount_ (QuerySelector ".im")  {
                view: SIV.view,
                init: Nothing,
                update
        }

update :: World IMModel IMMessage -> IMModel -> IMMessage -> Aff IMModel
update world model =
                case _ of
                        SM message -> CIS.update world model message