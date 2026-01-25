module Server.Effect where

import Prelude

import Data.HashMap (HashMap)
import Droplet.Driver (Pool)
import Effect.Class (liftEffect)
import Effect.Console as EC
import Effect.Ref (Ref)
import Run (AFF, Run, EFFECT)
import Run as R
import Run.Except (EXCEPT)
import Run.Except as RE
import Run.Reader (READER)
import Run.Reader as RR
import Shared.ResponseError (ResponseError)
import Type.Row (type (+))

type BaseReader extension =
      { pool ∷ Pool
      | extension
      }

type ServerReader = BaseReader
      ( allUserSubscriptionsRef ∷ Ref (HashMap Int (Array String))
      )

type BaseEffect r a = Run (READER r + EXCEPT ResponseError + AFF + EFFECT + ()) a

type ServerEffect a = BaseEffect ServerReader a

poolEffect pool d = R.runBaseAff' <<< RE.catch (\e → liftEffect (EC.logShow e) *> pure d) <<< RR.runReader { pool }